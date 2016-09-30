{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  #-}

module Monero.Wallet.Process where

import Data.Process
import Data.Json.RPC
import Monero.Wallet.RPC

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (void)

import System.FilePath
import System.IO
import Control.Concurrent (threadDelay)
import Network (PortNumber)



-- | Global config options for all wallets
data WalletProcessConfig = WalletProcessConfig
  { walletsDir          :: FilePath -- ^ Directory where wallets are stored
  , moneroWalletCliPath :: FilePath -- ^ Executable path for `monero-wallet-cli`
  , walletRpcPort       :: PortNumber
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


-- * Wallet Creation

data MakeWalletConfig = MakeWalletConfig
  { makeWalletName     :: T.Text -- ^ Names need to be unique
  , makeWalletPassword :: T.Text
  , makeWalletLanguage :: WalletLanguage
  } deriving (Show, Eq)


makeWallet :: WalletProcessConfig
           -> MakeWalletConfig
           -> IO (RPCConfig, ProcessHandles)
makeWallet WalletProcessConfig{..} MakeWalletConfig{..} = do
  let name' = walletsDir </> T.unpack makeWalletName
      args = [ "--generate-new-wallet=" ++ name'
             , "--log-file=" ++ name' ++ ".log"
             , "--password=" ++ T.unpack makeWalletPassword
             , "--rpc-bind-port=" ++ show (fromIntegral walletRpcPort :: Int)
             ]
  hs@ProcessHandles{..} <- mkProcess moneroWalletCliPath args

  threadDelay (2 * second)
  T.hPutStrLn stdinHandle . T.pack . show $ walletLanguageCode makeWalletLanguage
  putStrLn "language in"

  cfg <- newRPCConfig "127.0.0.1" walletRpcPort
  pure (cfg, hs)


-- * Opening Existing Wallet

data OpenWalletConfig = OpenWalletConfig
  { openWalletName     :: T.Text
  , openWalletPassword :: T.Text
  } deriving (Show, Eq)


openWallet :: WalletProcessConfig
           -> OpenWalletConfig
           -> IO (RPCConfig, ProcessHandles)
openWallet WalletProcessConfig{..} OpenWalletConfig{..} = do
  let name' = walletsDir </> T.unpack openWalletName
      args = [ "--wallet-file=" ++ name'
             , "--log-file=" ++ name' ++ ".log"
             , "--password=" ++ T.unpack openWalletPassword
             , "--rpc-bind-port=" ++ show (fromIntegral walletRpcPort :: Int)
             ]
  hs@ProcessHandles{..} <- mkProcess moneroWalletCliPath args

  cfg <- newRPCConfig "127.0.0.1" walletRpcPort
  pure (cfg, hs)


-- * Close Wallet Connection

closeWallet :: RPCConfig
            -> ProcessHandles
            -> IO ()
closeWallet cfg ProcessHandles{..} = do
  void $ stopWallet cfg
  mapM_ hClose [stdinHandle, stdoutHandle, stderrHandle]


second :: Int
second = 1000000
