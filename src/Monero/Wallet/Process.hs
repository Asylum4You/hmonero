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
import Control.Monad.Catch
import Control.Monad (void)
import Control.Concurrent (threadDelay)

import System.FilePath
import System.IO
import System.IO.Error (isEOFError)
import System.Directory (getCurrentDirectory)
import System.Process (waitForProcess, interruptProcessGroupOf)
import System.IDontNotify (neglectFile)
import Network (HostName, PortNumber)



-- | Global config options for all wallets
data WalletProcessConfig = WalletProcessConfig
  { walletsDir          :: FilePath -- ^ Directory where wallets are stored
  , moneroWalletCliPath :: FilePath -- ^ Executable path for `monero-wallet-cli`
  , walletRpcIp         :: IPv4
  , walletRpcPort       :: PortNumber
  , walletDaemonHost    :: HostName
  , walletDaemonPort    :: PortNumber
  } deriving (Show, Eq)


instance Default WalletProcessConfig where
  def = WalletProcessConfig
          { walletsDir = "."
          , moneroWalletCliPath = "monero-wallet-cli"
          , walletRpcIp = "127.0.0.1"
          , walletRpcPort = 18082
          , walletDaemonHost = "node.moneybit.science"
          , walletDaemonPort = 18081
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


-- TODO: Recover from mnemonic
makeWallet :: WalletProcessConfig
           -> MakeWalletConfig
           -> IO ()
makeWallet WalletProcessConfig{..} MakeWalletConfig{..} = do
  dir <- if walletsDir == "."
         then getCurrentDirectory
         else pure walletsDir
  let name' = dir </> T.unpack makeWalletName
      args = [ "--generate-new-wallet=" ++ name'
             , "--log-file="            ++ name' ++ ".log"
             , "--log-level=2"
             , "--password="            ++ T.unpack makeWalletPassword
             -- , "--rpc-bind-ip="         ++ show walletRpcIp
             -- , "--rpc-bind-port"        ++ show (fromIntegral walletRpcPort :: Int)
             , "--daemon-host="         ++ walletDaemonHost
             , "--daemon-port="         ++ show (fromIntegral walletDaemonPort :: Int)
             ]
  ProcessHandles{..} <- mkProcess moneroWalletCliPath args

  threadDelay (2 * second)
  T.hPutStrLn stdinHandle . T.pack . show $ walletLanguageCode makeWalletLanguage
  hFlush stdinHandle

  threadDelay (2 * second) -- FIXME: don't start neglecting until active?
  neglectFile (name' ++ ".log") second

  interruptProcessGroupOf processHandle
  mapM_ hClose [stdinHandle, stdoutHandle, stderrHandle]


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
      args = [ "--wallet-file="   ++ name'
             , "--log-file="      ++ name' ++ ".log"
             , "--log-level=2"
             , "--password="      ++ T.unpack openWalletPassword
             , "--rpc-bind-ip="   ++ show walletRpcIp
             , "--rpc-bind-port=" ++ show (fromIntegral walletRpcPort :: Int)
             , "--daemon-host="   ++ walletDaemonHost
             , "--daemon-port="   ++ show (fromIntegral walletDaemonPort :: Int)
             ]
  hs@ProcessHandles{..} <- mkProcess moneroWalletCliPath args

  threadDelay (2 * second)
  neglectFile (name' ++ ".log") second

  cfg <- newRPCConfig walletRpcIp walletRpcPort
  pure (cfg, hs)


-- * Close Wallet Connection

closeWallet :: ProcessHandles
            -> IO ()
closeWallet ProcessHandles{..} =
  bracket_ (pure ())
    (mapM_ hClose [stdinHandle, stdoutHandle, stderrHandle])
    $ do  interruptProcessGroupOf processHandle
          void $ waitForProcess processHandle
         -- case e of
         --   ExitSuccess ->
         --   ExitFailure i ->
         --     pure () -- throwM $ NonZeroExitCode i -- FIXME: http://unix.stackexchange.com/questions/99112/default-exit-code-when-process-is-terminated


second :: Int
second = 1000000

-- * Utils
