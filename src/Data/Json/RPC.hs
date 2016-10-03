{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , DeriveGeneric
  , GADTs
  , StandaloneDeriving
  , FlexibleContexts
  #-}

{-|
Module : Data.Json.RPC
Copyright : (c) 2016 Athan Lawrence Clark
License : BSD-style
Maintainer : athan.clark@gmail.com
Stability : experimental
Portability : GHC

A simple abstraction over JSON RPC 2.0 for use with monero's
simplewallet and monerod.
-}

module Data.Json.RPC where

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.STRef
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Data.IP (IPv4)
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.ST

import Network (PortNumber)
import Network.HTTP.Client ( Manager, newManager, defaultManagerSettings
                           , httpLbs, parseUrl, RequestBody (RequestBodyLBS)
                           , requestBody, requestHeaders, method, responseBody
                           )


-- * RPC Interface

data RPCRequest ps = RPCRequest
  { rpcReqMethod  :: T.Text -- ^ User Data
  , rpcReqParams  :: Maybe ps
  , rpcReqId      :: Int
  } deriving (Show, Eq)

instance ToJSON ps => ToJSON (RPCRequest ps) where
  toJSON RPCRequest{..} =
    object $
      [ "jsonrpc" .= ("2.0" :: T.Text)
      , "method"  .= rpcReqMethod
      , "id"      .= show rpcReqId
      ] ++ case rpcReqParams of
             Nothing -> []
             Just ps -> ["params" .= ps]




data RPCResponseError = RPCResponseError
  { rpcErrorCode    :: Int
  , rpcErrorMessage :: T.Text
  , rpcErrorData    :: Maybe A.Value
  } deriving (Show, Eq)

instance FromJSON RPCResponseError where
  parseJSON (Object o) = do
    c <- o .:  "code"
    m <- o .:  "message"
    d <- o .:? "data"
    pure $ RPCResponseError c m d
  parseJSON x = typeMismatch "RPCResponseError" x



data RPCResponse rs = RPCResponse
  { rpcRespId     :: Int
  , rpcRespResult :: Either RPCResponseError rs
  } deriving (Show, Eq)

instance FromJSON rs => FromJSON (RPCResponse rs) where
  parseJSON (Object o) = do
    v <- o .: "jsonrpc"
    if v /= ("2.0" :: T.Text)
    then fail "Not 2.0 version"
    else do
      i <- o .: "id"
      r <-  (Left  <$> o .: "error")
        <|> (Right <$> o .: "result")
      case readMaybe $ T.unpack i of
        Nothing -> fail "id isn't an Int string"
        Just i  -> pure $ RPCResponse i r
  parseJSON x = typeMismatch "RPCResponse" x


-- * Invoking an RPC


data RPCConfig = RPCConfig
  { rpcIp       :: IPv4
  , rpcPort     :: PortNumber
  , rpcManager  :: Manager
  , rpcId       :: STRef RealWorld Int
  }


newRPCConfig :: IPv4 -> PortNumber -> IO RPCConfig
newRPCConfig ip p = do
  m <- newManager defaultManagerSettings
  i <- stToIO $ newSTRef 0
  pure RPCConfig
    { rpcIp       = ip
    , rpcPort     = p
    , rpcManager  = m
    , rpcId       = i
    }


-- | Invoke an RPC call. See "Monero.Client.RPC" for usage.
rpc :: ( ToJSON ps
       , Show ps
       , FromJSON rs
       ) => RPCConfig
         -> T.Text -- ^ Method
         -> Maybe ps -- ^ Params
         -> IO rs
rpc RPCConfig{..} method mx = do
  let host = show rpcIp ++ ":" ++ show (fromIntegral rpcPort :: Int)
  r   <- parseUrl $ "http://" ++ host ++ "/json_rpc" -- FIXME Tls support
  idx <- freshId rpcId
  let req = RPCRequest
              { rpcReqMethod = method
              , rpcReqParams = mx
              , rpcReqId     = idx
              }
      r' = r
              { requestHeaders = [("Content-Type","application/json")]
              , requestBody = RequestBodyLBS $ A.encode req
              , method = "POST"
              }
  response <- liftIO $ httpLbs r' rpcManager

  let body = responseBody response
  case A.eitherDecode body of
    Left e -> throwM $ MalformedRPCData e body
    Right resp -> do
      let idx' = rpcRespId resp
      if idx' /= idx
      then throwM $ MismatchingIds idx idx' req
      else case rpcRespResult resp of
        Left  e -> throwM $ RPCError e
        Right y -> pure y


-- ** Utils

freshId :: STRef RealWorld Int -> IO Int
freshId rpcId = stToIO $ do
  idx <- readSTRef rpcId
  writeSTRef rpcId $! idx + 1
  pure idx

-- monomorphism restriction in existentally quantified types?
nada :: Maybe ()
nada = Nothing



-- * Exceptions

data RPCException where
  MalformedRPCData :: String -> LBS.ByteString -> RPCException
  MismatchingIds   :: Show a => Int -> Int -> RPCRequest a -> RPCException
  RPCError         :: RPCResponseError -> RPCException

deriving instance Show RPCException
instance Exception RPCException

