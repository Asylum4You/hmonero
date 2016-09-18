{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , RecordWildCards
  #-}

{-|
Module : Monero.Daemon.RPC
Copyright : (c) 2016 Athan Lawrence Clark
License : BSD-style
Maintainer : athan.clark@gmail.com
Stability : experimental
Portability : GHC

Verbatim copy of
<https://getmonero.org/knowledge-base/developer-guides/wallet-rpc simplewallet's RPC endpoints>.
-}

module Monero.Daemon.RPC where

import Monero.Types
import Data.Json.RPC

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT


-- * Procedures

-- ** Get Block Count

data GotBlockCount = GotBlockCount
  { gotBlockCount       :: Word64
  , gotBlockCountStatus :: T.Text
  } deriving (Show, Eq)
instance FromJSON GotBlockCount where
  parseJSON (Object o) =
    GotBlockCount <$> o .: "count"
                  <*> o .: "status" -- FIXME: Should be "OK"
  parseJSON x = typeMismatch "GotBlockCount" x

getBlockCount :: RPCConfig -> IO GotBlockCount
getBlockCount cfg = rpc cfg "getblockcount" nada

-- ** On Get Block Hash

-- type BlockHeight = [Word64]
-- FIXME: Ask if the array of length 1 is legit
onGetBlockHash :: RPCConfig
               -> Word64 -- ^ Block height
               -> IO HexString
onGetBlockHash cfg h = rpc cfg "on_getblockhash" $ Just [h]

-- ** Get Block Template

data GetBlockTemplate = GetBlockTemplate
  { getBlockTemplateWalletAddr  :: Address
  , getBlockTemplateReserveSize :: Word64
  } deriving (Show, Eq)
instance ToJSON GetBlockTemplate where
  toJSON GetBlockTemplate{..} = object
    [ "wallet_address" .= getBlockTemplateWalletAddr
    , "reserve_size" .= getBlockTemplateReserveSize
    ]

data GotBlockTemplate = GotBlockTemplate
  { gotBlockTemplateBlob           :: HexString
  , gotBlockTemplateDifficulty     :: Word64
  , gotBlockTemplateHeight         :: Word64
  , gotBlockTemplatePrevHash       :: HexString
  , gotBlockTemplateReservedOffset :: Word64
  , gotBlockTemplateStatus         :: T.Text -- FIXME: Should be "OK"
  } deriving (Show, Eq)
instance FromJSON GotBlockTemplate where
  parseJSON (Object o) =
    GotBlockTemplate <$> o .: "blocktemplate_blob"
                     <*> o .: "difficulty"
                     <*> o .: "height"
                     <*> o .: "prev_hash"
                     <*> o .: "reserved_offset"
                     <*> o .: "status"
  parseJSON x = typeMismatch "GotBlockTemplate" x

getBlockTemplate :: RPCConfig -> GetBlockTemplate -> IO GotBlockTemplate
getBlockTemplate cfg g = rpc cfg "getblocktemplate" $ Just g

-- ** Submit Block

-- FIXME: Should have an example
submitBlock :: RPCConfig
            -> HexString -- ^ Block blob data
            -> IO T.Text -- FIXME: Status
submitBlock cfg h = rpc cfg "submitblock" $ Just h

-- ** Get Last Block Header

data GotBlockHeader = GotBlockHeader
  { gotBlockHeader       :: BlockHeader
  , gotBlockHeaderStatus :: T.Text -- FIXME: Should be "OK"
  } deriving (Show, Eq)
instance FromJSON GotBlockHeader where
  parseJSON (Object o) =
    GotBlockHeader <$> o .: "block_header"
                   <*> o .: "status"
  parseJSON x = typeMismatch "GotBlockHeader" x

getLastBlockHeader :: RPCConfig -> IO GotBlockHeader
getLastBlockHeader cfg = rpc cfg "getlastblockheader" nada

-- ** Get Block Header By Hash

newtype GetBlockHeaderByHash = GetBlockHeaderByHash
  { getBlockHeaderByHash' :: HexString
  } deriving (Show, Eq)
instance ToJSON GetBlockHeaderByHash where
  toJSON GetBlockHeaderByHash{..} = object ["hash" .= getBlockHeaderByHash']

getBlockHeaderByHash :: RPCConfig -> GetBlockHeaderByHash -> IO GotBlockHeader
getBlockHeaderByHash cfg h = rpc cfg "getblockheaderbyhash" $ Just h

-- ** Get Block Header By Height

newtype GetBlockHeaderByHeight = GetBlockHeaderByHeight
  { getBlockHeaderByHeight' :: Word64
  } deriving (Show, Eq)
instance ToJSON GetBlockHeaderByHeight where
  toJSON GetBlockHeaderByHeight{..} = object ["height" .= getBlockHeaderByHeight']

getBlockHeaderByHeight :: RPCConfig -> GetBlockHeaderByHeight -> IO GotBlockHeader
getBlockHeaderByHeight cfg h = rpc cfg "getblockheaderbyheight" $ Just h

-- ** Get Block

data GetBlock
  = GetBlockByHeight Word64
  | GetBlockByHash HexString
  deriving (Show, Eq)
instance ToJSON GetBlock where
  toJSON (GetBlockByHeight h) = object ["height" .= h]
  toJSON (GetBlockByHash   h) = object ["hash"   .= h]

data GotBlock = GotBlock
  { gotBlockBlob :: HexString
  , gotBlockHeader' :: BlockHeader
  , gotBlockDetails :: BlockDetails
  , gotBlockStatus  :: T.Text
  } deriving (Show, Eq)
instance FromJSON GotBlock where
  parseJSON x@(Object o) = do
    b <- o .: "blob"
    h <- o .: "block_header"
    j <- o .: "json"
    s <- o .: "status"
    case A.decode $ LT.encodeUtf8 j of
      Nothing -> typeMismatch "GotBlock" x
      Just ds -> pure $ GotBlock b h ds s
  parseJSON x = typeMismatch "GotBlock" x

getBlock :: RPCConfig -> GetBlock -> IO GotBlock
getBlock cfg g = rpc cfg "getblock" $ Just g


-- ** Get Connections

newtype GotConnections = GotConnections
  { gotConnections :: [Connection]
  } deriving (Show, Eq)
instance FromJSON GotConnections where
  parseJSON (Object o) = GotConnections <$> o .: "connections"
  parseJSON x = typeMismatch "GotConnections" x

getConnections :: RPCConfig -> IO GotConnections
getConnections cfg = rpc cfg "get_connections" nada

-- ** Get Info

data GotInfo = GotInfo
  { gotInfoAltBlocksCount    :: Word64
  , gotInfoDifficulty        :: Word64
  , gotInfoGreyPeerlistSize  :: Word64
  , gotInfoHeight            :: Word64
  , gotInfoIncomingConnCount :: Word64
  , gotInfoOutgoingConnCount :: Word64
  , gotInfoStatus            :: T.Text
  , gotInfoTarget            :: Word64
  , gotInfoTargetHeight      :: Word64
  , gotInfoTestnet           :: Bool
  , gotInfoTopBlockHash      :: T.Text
  , gotInfoTxCount           :: Word64
  , gotInfoTxPoolSize        :: Word64
  , gotInfoWhitePeerlistSize :: Word64
  } deriving (Show, Eq)
instance FromJSON GotInfo where
  parseJSON (Object o) =
    GotInfo <$> o .: "alt_blocks_count"
            <*> o .: "difficulty"
            <*> o .: "grey_peerlist_size"
            <*> o .: "height"
            <*> o .: "incoming_connections_count"
            <*> o .: "outgoing_connections_count"
            <*> o .: "status"
            <*> o .: "target"
            <*> o .: "target_height"
            <*> o .: "testnet"
            <*> o .: "top_block_hash"
            <*> o .: "tx_count"
            <*> o .: "tx_pool_size"
            <*> o .: "white_peerlist_count"
  parseJSON x = typeMismatch "GotInfo" x

getInfo :: RPCConfig -> IO GotInfo
getInfo cfg = rpc cfg "get_info" nada

-- ** Hard Fork Info

data GotHardForkInfo = GotHardForkInfo
  { gotHFInfoEarliestHeight :: Word64
  , gotHFInfoEnabled        :: Bool
  , gotHFInfoState          :: Word64
  , gotHFInfoStatus         :: T.Text
  , gotHFInfoThreshold      :: Word64
  , gotHFInfoVersion        :: Word64
  , gotHFInfoVotes          :: Word64
  , gotHFInfoVoting         :: Word64
  , gotHFInfoWindow         :: Word64
  } deriving (Show, Eq)
instance FromJSON GotHardForkInfo where
  parseJSON (Object o) =
    GotHardForkInfo <$> o .: "earliest_height"
                    <*> o .: "enabled"
                    <*> o .: "state"
                    <*> o .: "status"
                    <*> o .: "threshold"
                    <*> o .: "version"
                    <*> o .: "votes"
                    <*> o .: "voting"
                    <*> o .: "window"
  parseJSON x = typeMismatch "GotHardForkInfo" x

hardForkInfo :: RPCConfig -> IO GotHardForkInfo
hardForkInfo cfg = rpc cfg "hard_fork_info" nada

-- ** Set Bans

data SetBan = SetBan
  { setBanIp  :: Word64
  , setBanBan :: Bool
  , setBanSec :: Word64
  } deriving (Show, Eq)
instance ToJSON SetBan where
  toJSON SetBan{..} = object
    [ "ip" .= setBanIp
    , "ban" .= setBanBan
    , "seconds" .= setBanSec
    ]

newtype SetBans = SetBans [SetBan]
instance ToJSON SetBans where
  toJSON (SetBans ss) = object ["bans" .= ss]

newtype DidSetBan = DidSetBan T.Text -- FIXME: RPC Status
instance FromJSON DidSetBan where
  parseJSON (Object o) = DidSetBan <$> o .: "status"
  parseJSON x = typeMismatch "DidSetBan" x

setBans :: RPCConfig -> [SetBan] -> IO DidSetBan
setBans cfg s = rpc cfg "set_bans" $ Just s

-- ** Get Bans

data GetBan = GetBan
  { getBanIp  :: Word64
  , getBanSec :: Word64
  } deriving (Show, Eq)
instance FromJSON GetBan where
  parseJSON (Object o) =
    GetBan <$> o .: "ip" <*> o .: "seconds"
  parseJSON x = typeMismatch "GetBan" x

data GetBans = GetBans
  { getBansList :: [GetBan]
  , getBansStatus :: T.Text
  } deriving (Show, Eq)
instance FromJSON GetBans where
  parseJSON (Object o) =
    GetBans <$> o .: "bans" <*> o .: "status"
  parseJSON x = typeMismatch "GetBans" x

getBans :: RPCConfig -> IO GetBans
getBans cfg = rpc cfg "get_bans" nada

-- * Other Daemon RPC Calls

-- TODO / FIXME - WONTFIX?
