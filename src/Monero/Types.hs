{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , RecordWildCards
  #-}

module Monero.Types where

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.String (IsString)
import Data.Word (Word64)
import Data.Char (isHexDigit)
import Data.Monoid
import qualified Data.Text as T



data Balance = Balance
  { balance         :: Word64
  , unlockedBalance :: Word64
  } deriving (Show, Eq)

instance FromJSON Balance where
  parseJSON (Object o) = do
    b <- o .: "balance"
    u <- o .: "unlocked_balance"
    pure $ Balance b u
  parseJSON x = typeMismatch "GotBalance" x


newtype Address = Address
  { getAddress :: T.Text
  } deriving (Show, Eq, FromJSON, ToJSON)


newtype PaymentId = PaymentId
  { getPaymentId :: T.Text
  } deriving (Show, Eq, ToJSON, IsString)
instance FromJSON PaymentId where
  parseJSON x = (PaymentId . getHexString) <$> parseJSON x


newtype TxHash = TxHash
  { getTxHash :: T.Text
  } deriving (Show, Eq)

instance ToJSON TxHash where
  toJSON TxHash{..} = String $! "<" <> getTxHash <> ">"
instance FromJSON TxHash where
  parseJSON (String s) =
    case T.uncons s of
      Just (l,s') | l == '<'
                 && T.length s' > 0
                 && T.last s' == '>'
                  ->
        let s'' = T.dropEnd 1 s'
        in  (TxHash . getHexString) <$> parseJSON (String s'')
      _ -> fail "improperly formatted"
  parseJSON x = typeMismatch "TxHash" x


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


-- * Utils

newtype HexString = HexString
  { getHexString :: T.Text
  } deriving (Show, Eq, ToJSON)
instance FromJSON HexString where
  parseJSON (String s) | T.all isHexDigit s = pure $ HexString s
                       | otherwise = fail "Not hexadecimal"
  parseJSON x = typeMismatch "HexString" x
