{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , RecordWildCards
  #-}

module Monero.Client.RPC where

{-|
Module : Monero.Client.RPC
Copyright : (c) 2016 Athan Lawrence Clark
License : BSD-style
Maintainer : athan.clark@gmail.com
Stability : experimental
Portability : GHC

Verbatim copy of
<https://getmonero.org/knowledge-base/developer-guides/wallet-rpc simplewallet's RPC endpoints>.

-}

import Monero.Types
import Data.Json.RPC

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified Data.Text as T


-- * Procedures

-- ** Get Balance

getBalance :: RPCConfig -> IO Balance
getBalance cfg = rpc cfg "getbalance" nada

-- ** Get Address

newtype GotAddress = GotAddress Address
instance FromJSON GotAddress where
  parseJSON (Object o) = GotAddress <$> o .: "address"
  parseJSON x = typeMismatch "GotAddress" x

getAddress :: RPCConfig -> IO GotAddress
getAddress cfg = rpc cfg "getaddress" nada

-- ** Get Height

newtype GotHeight = GotHeight Int
instance FromJSON GotHeight where
  parseJSON (Object o) = GotHeight <$> o .: "height"
  parseJSON x = typeMismatch "GotHeight" x

getHeight :: RPCConfig -> IO GotHeight
getHeight cfg = rpc cfg "getheight" nada

-- ** Make Transfer

data MakeTransfer = MakeTransfer
  { makeTransferDestinations :: [TransferDestination]
  , makeTransferMixin        :: Word64
  , makeTransferUnlockTime   :: Word64
  , makeTransferPaymentId    :: Maybe PaymentId
  , makeTransferGetTxKey     :: Bool
  } deriving (Show, Eq)
  -- NOTE: fee is obsolete
instance ToJSON MakeTransfer where
  toJSON MakeTransfer{..} = object $
    [ "destinations" .= makeTransferDestinations
    , "mixin"        .= makeTransferMixin
    , "unlock_time"  .= makeTransferUnlockTime
    , "get_tx_key"   .= makeTransferGetTxKey
    ] ++ case makeTransferPaymentId of
           Nothing -> []
           Just p  -> ["payment_id" .= p]

data MadeTransfer = MadeTransfer
  { transactionHash :: TxHash
  , transactionKey  :: Maybe T.Text -- FIXME transaction key
  } deriving (Show, Eq)
instance FromJSON MadeTransfer where
  parseJSON (Object o) =
    MadeTransfer <$> o .:  "tx_hash"
                 <*> o .:? "tx_key"
  parseJSON x = typeMismatch "MadeTransfer" x

transfer :: RPCConfig -> MakeTransfer -> IO MadeTransfer
transfer cfg t = rpc cfg "transfer" $ Just t


-- ** Make Transfer Split

data MakeTransferSplit = MakeTransferSplit
  { makeTransferSplitDestinations :: [TransferDestination]
  , makeTransferSplitMixin        :: Word64
  , makeTransferSplitUnlockTime   :: Word64
  , makeTransferSplitPaymentId    :: Maybe PaymentId
  , makeTransferSplitGetTxKey     :: Bool
  , makeTransferSplitNewAlgorithm :: Bool
  } deriving (Show, Eq)
  -- NOTE: fee is obsolete
instance ToJSON MakeTransferSplit where
  toJSON MakeTransferSplit{..} = object $
    [ "destinations"  .= makeTransferSplitDestinations
    , "mixin"         .= makeTransferSplitMixin
    , "unlock_time"   .= makeTransferSplitUnlockTime
    , "get_tx_key"    .= makeTransferSplitGetTxKey
    , "new_algorithm" .= makeTransferSplitNewAlgorithm
    ] ++ case makeTransferSplitPaymentId of
           Nothing -> []
           Just p  -> ["payment_id" .= p]

data MadeTransferSplit = MadeTransferSplit
  { transactionHashList :: [TxHash] -- FIXME no keys?
  } deriving (Show, Eq)
instance FromJSON MadeTransferSplit where
  parseJSON (Object o) = MadeTransferSplit <$> o .: "tx_hash_list"
  parseJSON x = typeMismatch "MadeTransferSplit" x

transferSplit :: RPCConfig -> MakeTransferSplit -> IO MadeTransferSplit
transferSplit cfg t = rpc cfg "transfer_split" $ Just t


-- ** Sweep Dust

data SweptDust = SweptDust
  { sweptTxHashList :: [TxHash]
  } deriving (Show, Eq)
instance FromJSON SweptDust where
  parseJSON (Object o) = SweptDust <$> o .: "tx_hash_list"
  parseJSON x = typeMismatch "SweptDust" x

sweepDust :: RPCConfig -> IO SweptDust
sweepDust cfg = rpc cfg "sweep_dust" nada


-- ** Store

store :: RPCConfig -> IO EmptyObject
store cfg = rpc cfg "store" nada


-- ** Get Payments

newtype GetPayments = GetPayments
  { getPaymentsId :: PaymentId
  } deriving (Show, Eq)
instance ToJSON GetPayments where
  toJSON GetPayments{..} = object ["payment_id" .= getPaymentsId]

newtype GotPayments = GotPayments
  { gotPayments :: [Payment]
  } deriving (Show, Eq)
instance FromJSON GotPayments where
  parseJSON (Object o) = GotPayments <$> o .: "payments"
  parseJSON x = typeMismatch "GotPayments" x

getPayments :: RPCConfig -> GetPayments -> IO GotPayments
getPayments cfg g = rpc cfg "get_payments" $ Just g


-- ** Get Bulk Payments

data GetBulkPayments = GetBulkPayments
  { bulkPaymentIds            :: [PaymentId]
  , bulkPaymentMinBlockHeight :: Word64
  } deriving (Show, Eq)
instance ToJSON GetBulkPayments where
  toJSON GetBulkPayments{..} = object
    [ "payment_ids" .= bulkPaymentIds
    , "min_block_height" .= bulkPaymentMinBlockHeight
    ]

newtype GotBulkPayments = GotBulkPayments
  { gotBulkPayments :: [Payment]
  } deriving (Show, Eq)
instance FromJSON GotBulkPayments where
  parseJSON (Object o) = GotBulkPayments <$> o .: "payments"
  parseJSON x = typeMismatch "GotBulkPayments" x

getBulkPayments :: RPCConfig -> GetBulkPayments -> IO GotBulkPayments
getBulkPayments cfg g = rpc cfg "get_bulk_payments" $ Just g


-- ** Incoming Transfers

data IncomingTransferType
  = AllTransferTypes
  | AvailableTransfers
  | UnavailableTransfers
  deriving (Show, Eq)
instance ToJSON IncomingTransferType where
  toJSON AllTransferTypes     = String "all"
  toJSON AvailableTransfers   = String "available"
  toJSON UnavailableTransfers = String "unavailable"

newtype GetIncomingTransfers = GetIncomingTransfers
  { getIncomingTransfers :: IncomingTransferType
  } deriving (Show, Eq)
instance ToJSON GetIncomingTransfers where
  toJSON GetIncomingTransfers{..} = object ["transfer_type" .= getIncomingTransfers]

newtype GotIncomingTransfers = GotIncomingTransfers
  { gotIncomingTransfers :: [Transfer]
  } deriving (Show, Eq)
instance FromJSON GotIncomingTransfers where
  parseJSON (Object o) = GotIncomingTransfers <$> o .:? "transfers" .!= [] -- Might not exist
  parseJSON x = typeMismatch "GotIncomingTransfers" x

incomingTransfers :: RPCConfig -> GetIncomingTransfers -> IO GotIncomingTransfers
incomingTransfers cfg g = rpc cfg "incoming_transfers" $ Just g

-- ** Query Key

data QueryKeyType
  = KeyMnemonic
  | KeyView
  deriving (Show, Eq)
instance ToJSON QueryKeyType where
  toJSON KeyMnemonic = String "mnemonic"
  toJSON KeyView     = String "view_key"

newtype QueryKey = QueryKey
  { queryKeyType :: QueryKeyType
  } deriving (Show, Eq)
instance ToJSON QueryKey where
  toJSON QueryKey{..} = object ["key_type" .= queryKeyType]

{-
data QueriedKey
  = ViewKey Address
  | Mnemonic T.Text
  deriving (Show, Eq)
instance FromJSON QueriedKey where
  parseJSON (String s) -} -- FIXME attoparsec hexadecimal

queryKey :: RPCConfig -> QueryKey -> IO T.Text -- FIXME account for either
queryKey cfg q = rpc cfg "query_key" $ Just q


-- ** Make Integrated Address

newtype MakeIntegratedAddress = MakeIntegratedAddress
  { makeIntegratedAddress' :: Maybe PaymentId
  } deriving (Show, Eq)
instance ToJSON MakeIntegratedAddress where
  toJSON MakeIntegratedAddress{..} = object
    ["payment_id" .= fromMaybe "" makeIntegratedAddress']

newtype MadeIntegratedAddress = MadeIntegratedAddress
  { madeIntegratedAddress :: Address
  } deriving (Show, Eq)
instance FromJSON MadeIntegratedAddress where
  parseJSON (Object o) = MadeIntegratedAddress <$> o .: "integrated_address"
  parseJSON x = typeMismatch "MadeIntegratedAddress" x

makeIntegratedAddress :: RPCConfig -> MakeIntegratedAddress -> IO MadeIntegratedAddress
makeIntegratedAddress cfg m = rpc cfg "make_integrated_address" $ Just m


-- ** Split Integrated Address

newtype SplitIntegratedAddress = SplitIntegratedAddress
  { splitIntegratedAddress' :: Address
  } deriving (Show, Eq)
instance ToJSON SplitIntegratedAddress where
  toJSON SplitIntegratedAddress{..} = object
    ["integrated_address" .= splitIntegratedAddress']

data GotSplitIntegratedAddress = GotSplitIntegratedAddress
  { gotSplitStandardAddress :: Address
  , gotSplitPaymentId       :: PaymentId
  } deriving (Show, Eq)
instance FromJSON GotSplitIntegratedAddress where
  parseJSON (Object o) =
    GotSplitIntegratedAddress <$> o .: "standard_address"
                              <*> o .: "payment_id"
  parseJSON x = typeMismatch "GotSplitIntegratedAddress" x

splitIntegratedAddress :: RPCConfig -> SplitIntegratedAddress
                       -> IO GotSplitIntegratedAddress
splitIntegratedAddress cfg s = rpc cfg "split_integrated_address" $ Just s


-- ** Stop Wallet

stopWallet :: RPCConfig -> IO EmptyObject
stopWallet cfg = rpc cfg "stop_wallet" nada


-- * Utils

data EmptyObject = EmptyObject
instance FromJSON EmptyObject where
  parseJSON (Object o) | o == mempty = pure EmptyObject
                       | otherwise   = fail "not an empty object"
  parseJSON x = typeMismatch "EmptyObject" x
