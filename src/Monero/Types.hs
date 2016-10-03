{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , RecordWildCards
  , ScopedTypeVariables
  #-}

{-|
Module : Monero.Types
Copyright : (c) 2016 Athan Lawrence Clark
License : BSD-style
Maintainer : athan.clark@gmail.com
Stability : experimental
Portability : GHC

Casual types occurring throughout simplewallet and monerod
-}

module Monero.Types where

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Word (Word64)
import Data.Char (isHexDigit, isAlphaNum, isAscii, toLower, ord, chr)
import Data.Monoid
import qualified Data.Text as T
import Network (PortNumber)
import Text.Read (readMaybe)

import Test.QuickCheck


-- * Components

newtype Address = Address
  { getAddress :: Base58String
  } deriving (Show, Eq, FromJSON, ToJSON, Arbitrary)


newtype PaymentId = PaymentId
  { getPaymentId :: HexString
  } deriving (Show, Eq, FromJSON, ToJSON, Arbitrary)


newtype TxHash = TxHash
  { getTxHash :: HexString
  } deriving (Show, Eq)

instance ToJSON TxHash where
  toJSON TxHash{..} = String $! "<" <> getHexString getTxHash <> ">"
instance FromJSON TxHash where
  parseJSON (String s) =
    case T.uncons s of
      Just (l,s') | l == '<'
                 && T.length s' > 0
                 && T.last s' == '>'
                  ->
        let s'' = T.dropEnd 1 s'
        in  TxHash <$> parseJSON (String s'')
      _ -> fail "improperly formatted"
  parseJSON x = typeMismatch "TxHash" x


-- * Slightly Larger Components

data Balance = Balance
  { balance         :: Word64
  , unlockedBalance :: Word64
  } deriving (Show, Eq)
instance Arbitrary Balance where
  arbitrary = Balance <$> arbitrary <*> arbitrary
  shrink Balance{..} = [ Balance b ub
                       | b  <- shrink balance
                       , ub <- shrink unlockedBalance
                       ]
instance FromJSON Balance where
  parseJSON (Object o) = do
    b <- o .: "balance"
    u <- o .: "unlocked_balance"
    pure $ Balance b u
  parseJSON x = typeMismatch "GotBalance" x


data Payment = Payment
  { paymentId          :: PaymentId
  , paymentTxHash      :: TxHash
  , paymentAmount      :: Word64
  , paymentBlockHeight :: Word64
  , paymentUnlockTime  :: Word64
  } deriving (Show, Eq)
instance FromJSON Payment where
  parseJSON (Object o) =
    Payment <$> o .: "payment_id"
            <*> o .: "tx_hash"
            <*> o .: "amount"
            <*> o .: "block_height"
            <*> o .: "unlock_time"
  parseJSON x = typeMismatch "Payment" x


data TransferDestination = TransferDestination
  { destinationAmount  :: Word64
  , destinationAddress :: Address
  } deriving (Show, Eq)

instance ToJSON TransferDestination where
  toJSON TransferDestination{..} = object
    [ "amount" .= destinationAmount
    , "address" .= destinationAddress
    ]

data Transfer = Transfer
  { transferAmount      :: Word64
  , transferSpent       :: Bool
  , transferGlobalIndex :: Word64
  , transferTxHash      :: TxHash
  , transferTxSize      :: Word64
  } deriving (Show, Eq)

instance FromJSON Transfer where
  parseJSON (Object o) =
    Transfer <$> o .: "amount"
             <*> o .: "spent"
             <*> o .: "global_index"
             <*> o .: "tx_hash"
             <*> o .: "tx_size"
  parseJSON x = typeMismatch "Transfer" x


data BlockHeader = BlockHeader
  { blockHeaderDepth        :: Word64
  , blockHeaderDifficulty   :: Word64
  , blockHeaderHash         :: HexString
  , blockHeaderHeight       :: Word64
  , blockHeaderMajorVersion :: Word64
  , blockHeaderMinorVersion :: Word64
  , blockHeaderNonce        :: Word64
  , blockHeaderOrphanStatus :: Bool
  , blockHeaderPrevHash     :: HexString
  , blockHeaderReward       :: Word64
  , blockHeaderTimestamp    :: Word64
  } deriving (Show, Eq)

instance FromJSON BlockHeader where
  parseJSON (Object o) =
    BlockHeader <$> o .: "depth"
                <*> o .: "difficulty"
                <*> o .: "hash"
                <*> o .: "height"
                <*> o .: "major_version"
                <*> o .: "minor_version"
                <*> o .: "nonce"
                <*> o .: "orphan_status"
                <*> o .: "prev_hash"
                <*> o .: "reward"
                <*> o .: "timestamp"
  parseJSON x = typeMismatch "BlockHeader" x


data TxDestination = TxDestination
  { txDestinationAmount :: Word64
  , txDestinationTarget :: HexString -- FIXME: TxHash?
  } deriving (Show, Eq)
instance FromJSON TxDestination where
  parseJSON (Object o) = do
    a <- o .: "amount"
    k <- do
      t <- o .: "target"
      case t of
        Object o -> o .: "key"
        x        -> typeMismatch "TxDestination" x
    pure $ TxDestination a k
  parseJSON x = typeMismatch "TxDestination" x


newtype TxInputHeight = TxInputHeight
  { getTxInputHeight :: Word64
  } deriving (Show, Eq)
instance FromJSON TxInputHeight where
  parseJSON (Object o) = do
    g <- o .: "gen"
    case g of
      Object o -> TxInputHeight <$> o .: "height"
      x        -> typeMismatch "TxInputHeight" x
  parseJSON x = typeMismatch "TxInputHeight" x


data MinerTx = MinerTx
  { minerTxVersion :: Word64
  , minerTxUnlockTime :: Word64 -- FIXME: Block height as a newtype
  , minerTxVin        :: [TxInputHeight] -- ^ is nested in an object
  , minerTxVout       :: [TxDestination]
  , minerTxExtra      :: [Word64]
  , minerTxSignatures :: [HexString]
  } deriving (Show, Eq)
instance FromJSON MinerTx where
  parseJSON (Object o) =
    MinerTx <$> o .: "version"
            <*> o .: "unlock_time"
            <*> o .: "vin"
            <*> o .: "vout"
            <*> o .: "extra"
            <*> o .: "signatures"
  parseJSON x = typeMismatch "MinerTx" x


data BlockDetails = BlockDetails
  { blockDetailsMajorVersion :: Word64
  , blockDetailsMinorVersion :: Word64
  , blockDetailsTimestamp    :: Word64
  , blockDetailsPrevId       :: HexString
  , blockDetailsNonce        :: Word64
  , blockDetailsMinerTx      :: MinerTx
  , blockDetailsTxHashes     :: [TxHash]
  } deriving (Show, Eq)

instance FromJSON BlockDetails where
  parseJSON (Object o) =
    BlockDetails <$> o .: "major_version"
                 <*> o .: "minor_version"
                 <*> o .: "timestamp"
                 <*> o .: "prev_id"
                 <*> o .: "nonce"
                 <*> o .: "miner_tx"
                 <*> o .: "tx_hashes"
  parseJSON x = typeMismatch "BlockDetails" x


data Connection = Connection
  { connectionAvgDownload     :: Word64
  , connectionAvgUpload       :: Word64
  , connectionCurrentDownload :: Word64
  , connectionCurrentUpload   :: Word64
  , connectionIncoming        :: Bool
  , connectionIp              :: T.Text -- FIXME: ipv4 or v6?
  , connectionLiveTime        :: Word64
  , connectionLocalIp         :: Bool
  , connectionLocalhost       :: Bool
  , connectionPeerId          :: HexString
  , connectionPort            :: PortNumber
  , connectionRecvCount       :: Word64
  , connectionRecvIdleTime    :: Word64
  , connectionSendCount       :: Word64
  , connectionSendIdleTime    :: Word64
  , connectionState           :: T.Text -- FIXME: Possible states?
  } deriving (Show, Eq)

instance FromJSON Connection where
  parseJSON x@(Object o) = do
    ad <- o .: "avg_download"
    au <- o .: "avg_upload"
    cd <- o .: "current_download"
    cu <- o .: "current_upload"
    i  <- o .: "incoming"
    ip <- o .: "ip"
    lt <- o .: "live_time"
    li <- o .: "local_ip"
    lh <- o .: "localhost"
    pi <- o .: "peer_id"
    p  <- do p' <- o .: "port"
             case readMaybe $ T.unpack p' of
               Nothing         -> typeMismatch "Connection" x
               Just (i :: Int) -> pure $ fromIntegral i
    rc <- o .: "recv_count"
    ri <- o .: "recv_idle_time"
    sc <- o .: "send_count"
    si <- o .: "send_idle_time"
    st <- o .: "state"
    pure Connection
      { connectionAvgDownload     = ad
      , connectionAvgUpload       = au
      , connectionCurrentDownload = cd
      , connectionCurrentUpload   = cu
      , connectionIncoming        = i
      , connectionIp              = ip
      , connectionLiveTime        = lt
      , connectionLocalIp         = li
      , connectionLocalhost       = lh
      , connectionPeerId          = pi
      , connectionPort            = p
      , connectionRecvCount       = rc
      , connectionRecvIdleTime    = ri
      , connectionSendCount       = sc
      , connectionSendIdleTime    = si
      , connectionState           = st
      }
  parseJSON x = typeMismatch "Connection" x


-- * Utils

newtype HexString = HexString
  { getHexString :: T.Text
  } deriving (Show, Eq, ToJSON)
instance FromJSON HexString where
  parseJSON (String s) | T.all isHexDigit s = pure $ HexString s
                       | otherwise          = fail "Not hexadecimal"
  parseJSON x = typeMismatch "HexString" x
instance Arbitrary HexString where
  arbitrary = (HexString . T.pack . map (toHex . toLower)) <$> arbitrary
    where
      toHex c | isHexDigit c = c
              | otherwise    = chr $ ord c `mod` 16
  shrink (HexString s) = map (HexString . T.pack) . shrink $ T.unpack s


newtype Base58String = Base58String
  { getBase58String :: T.Text
  } deriving (Show, Eq, ToJSON)
instance FromJSON Base58String where
  parseJSON (String s) | T.all isBase58Digit s = pure $ Base58String s
                       | otherwise = fail "Not base58"
    where
      isBase58Digit c = isAlphaNum c && isAscii c && isntMistakeable
        where
          isntMistakeable = c /= 'I' && c /= 'l' && c /= '0' && c /= 'O'
  parseJSON x = typeMismatch "HexString" x
instance Arbitrary Base58String where
  arbitrary = (Base58String . T.pack . map toBase58) <$> arbitrary
    where
      toBase58 c | isBase58Digit c = c
                 | otherwise       = 'A'
      isBase58Digit c = isAlphaNum c && isAscii c && isntMistakeable
        where
          isntMistakeable = c /= 'I' && c /= 'l' && c /= '0' && c /= 'O'
  shrink (Base58String s) = map (Base58String . T.pack) . shrink $ T.unpack s
