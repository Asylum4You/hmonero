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
import Control.Monad (void, unless, forM_, when, forever)
import Control.Concurrent (threadDelay, forkIO, killThread)

import System.FilePath
import System.IO
import System.IO.Error (isEOFError, isDoesNotExistError)
import System.Exit (ExitCode (..))
import System.Directory (getCurrentDirectory, removeFile)
import System.Process (waitForProcess, interruptProcessGroupOf
                      , readCreateProcessWithExitCode, shell)
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

  tryRemoveFile $ name' ++ ".log"
  tryRemoveFile $ name' ++ ".stdout.log"

  ProcessHandles{..} <- mkProcess moneroWalletCliPath args

  threadDelay (2 * second)
  T.hPutStrLn stdinHandle . T.pack . show $ walletLanguageCode makeWalletLanguage
  hFlush stdinHandle

  -- threadDelay (2 * second) -- FIXME: don't start neglecting until active?
  errWatcher <- forkIO $ throwLogging name'
  neglectFile (name' ++ ".log") second

  interruptProcessGroupOf processHandle
  mapM_ hClose [stdinHandle, stdoutHandle, stderrHandle]

  killThread errWatcher


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

  tryRemoveFile $ name' ++ ".log"
  tryRemoveFile $ name' ++ ".stdout.log"

  hs@ProcessHandles{stdoutHandle} <- mkProcess moneroWalletCliPath args

  stdLog <- hGetContents stdoutHandle
  stdLogger <- forkIO $ writeFile (name' ++ ".stdout.log") stdLog
  errWatcher <- forkIO $ throwLogging name'

  let loop = do
        threadDelay second
        xs <- T.lines <$> T.readFile (name' ++ ".log")
        unless (any ("Starting wallet rpc server" `T.isInfixOf`) xs) loop
  loop

  cfg <- newRPCConfig walletRpcIp walletRpcPort

  killThread stdLogger
  killThread errWatcher

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


tryRemoveFile :: FilePath -> IO ()
tryRemoveFile file =
  removeFile file `catch` handleError
  where
    handleError e | isDoesNotExistError e = pure ()
                  | otherwise             = throwM e


throwLogging :: FilePath -> IO ()
throwLogging name' = forever $ do
  threadDelay second
  xs <- T.lines <$> T.readFile (name' ++ ".log")
  forM_ xs $ \x -> when ("ERROR" `T.isInfixOf` x) $ do
                     putStrLn "Found ERROR, stopping..."
                     throwM $ LoggingError x


nextAvailPort :: PortNumber -> IO PortNumber
nextAvailPort startingPort = go startingPort
  where
    go p = do
      isAvail <- portIsAvail p
      if isAvail then pure p else go $ p + 1


portIsAvail :: PortNumber -> IO Bool
portIsAvail p = do
  (e,xs,_) <- readCreateProcessWithExitCode (shell $ "lsof -i :" ++ show p) ""
  case (e,xs) of
    (ExitFailure 1, "") -> pure True
    (ExitSuccess, _)    -> pure False
    _                   -> error $ "lsof failed: " ++ show (e,xs, "lsof -i :" ++ show p)
