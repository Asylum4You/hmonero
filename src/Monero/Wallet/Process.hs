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
import Data.IP (IPv4)
import Data.Default
import Control.Monad (void)

import System.FilePath
import System.IO
import System.Directory (getCurrentDirectory)
import Control.Concurrent (threadDelay)
import Network (PortNumber)



-- | Global config options for all wallets
data WalletProcessConfig = WalletProcessConfig
  { walletsDir          :: FilePath -- ^ Directory where wallets are stored
  , moneroWalletCliPath :: FilePath -- ^ Executable path for `monero-wallet-cli`
  , walletRpcIp         :: IPv4
  , walletRpcPort       :: PortNumber
  } deriving (Show, Eq)


instance Default WalletProcessConfig where
  def = WalletProcessConfig
          { walletsDir = "."
          , moneroWalletCliPath = "monero-wallet-cli"
          , walletRpcIp = "127.0.0.1"
          , walletRpcPort = 18082
          }


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
  dir <- if walletsDir == "."
         then getCurrentDirectory
         else pure walletsDir
  let name' = dir </> T.unpack makeWalletName
      args = [ "--generate-new-wallet=" ++ show name'
             , "--log-file=" ++ show (name' ++ ".log")
             , "--password=" ++ show (T.unpack makeWalletPassword)
             , "--rpc-bind-ip=" ++ show (show walletRpcIp)
             -- , "--rpc-bind-port"
             -- , show (fromIntegral walletRpcPort :: Int)
             ]
  let cmd = unwords $ moneroWalletCliPath : args
  putStrLn cmd
  hs@ProcessHandles{..} <- mkProcess cmd -- moneroWalletCliPath args

  threadDelay (2 * second)
  T.hPutStrLn stdinHandle . T.pack . show $ walletLanguageCode makeWalletLanguage
  hFlush stdinHandle
  putStrLn "language in"

  cfg <- newRPCConfig walletRpcIp walletRpcPort
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
  dir <- if walletsDir == "."
         then getCurrentDirectory
         else pure walletsDir
  let name' = dir </> T.unpack openWalletName
      args = [ "--wallet-file=" ++ show name'
             , "--log-file=" ++ show (name' ++ ".log")
             , "--password=" ++ show (T.unpack openWalletPassword)
             , "--rpc-bind-ip=" ++ show (show walletRpcIp)
             , "--rpc-bind-port=" ++ show (show (fromIntegral walletRpcPort :: Int))
             ]
  hs@ProcessHandles{..} <- mkProcess "" -- moneroWalletCliPath args

  cfg <- newRPCConfig walletRpcIp walletRpcPort
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

-- * Utils
