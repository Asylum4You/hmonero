{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  #-}

module Monero.Wallet.Process where

import Data.Process

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Catch

import System.FilePath
import System.IO
import System.IO.Error (isEOFError)
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)



-- | Global config options for all wallets
data WalletProcessConfig = WalletProcessConfig
  { walletsDir          :: FilePath -- ^ Directory where wallets are stored
  , moneroWalletCliPath :: FilePath -- ^ Executable path for `monero-wallet-cli`
  } deriving (Show, Eq)


data WalletLanguage
  = English
  | Spanish
  | German
  | Italian
  | Portuguese
  | Russian
  | Japanese
  deriving (Show, Eq)

walletLanguageCode :: WalletLanguage -> Int
walletLanguageCode l =
  case l of
    English    -> 0
    Spanish    -> 1
    German     -> 2
    Italian    -> 3
    Portuguese -> 4
    Russian    -> 5
    Japanese   -> 6


data MakeWalletConfig = MakeWalletConfig
  { walletName     :: T.Text -- ^ Names need to be unique
  , walletPassword :: T.Text
  , walletLanguage :: WalletLanguage
  } deriving (Show, Eq)


makeWallet :: WalletProcessConfig
           -> MakeWalletConfig
           -> IO ()
makeWallet WalletProcessConfig{..} MakeWalletConfig{..} = do
  let name' = walletsDir </> T.unpack walletName
      args = [ "--generate-new-wallet=" ++ name'
             , "--log-file=" ++ name' ++ ".log"
             ]
  ProcessHandles{..} <- mkProcess moneroWalletCliPath args

  waitFor ("Logging at log level " `T.isPrefixOf`) stdoutHandle
  T.hPutStrLn stdinHandle walletPassword
  threadDelay second
  T.hPutStrLn stdinHandle walletPassword
  putStrLn "passwords in"

  -- waitFor ("6 : Jap" `T.isPrefixOf`) stdoutHandle
  threadDelay (2 * second)
  T.hPutStrLn stdinHandle . T.pack . show $ walletLanguageCode walletLanguage
  putStrLn "language in"

  threadDelay (5 * second)
  T.hPutStrLn stdinHandle "exit"

  mapM_ hClose [stdinHandle, stdoutHandle, stderrHandle]


-- | blocks until the prompt is available
waitFor :: (T.Text -> Bool) -> Handle -> IO ()
waitFor isS h = do
  r <- timeout (60 * second) $ do
    threadDelay second
    ml <- getLastLine h
    print ml
    case ml of
      Just l | isS l -> pure ()
      _              -> waitFor isS h

  case r of
    Nothing -> throwM StdoutBlockTimeout
                 { stdoutBlockTimeoutHandle = h
                 }
    Just () -> pure ()


-- | grabs the last line in a handle, consuming
getLastLine :: Handle -> IO (Maybe T.Text)
getLastLine h =
  go Nothing False
  where
    go :: Maybe T.Text -> Bool -> IO (Maybe T.Text)
    go soFar True = pure soFar
    go soFar False =
      getLn `catch` (\e -> if isEOFError e
                           then go soFar True
                           else throwM e)
      where
        getLn :: IO (Maybe T.Text)
        getLn = do
          r <- timeout second $ T.hGetLine h
          case r of
            Nothing   -> go soFar True
            Just next -> go (Just next) False


second :: Int
second = 1000000
