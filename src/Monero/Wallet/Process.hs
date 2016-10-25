{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  , CPP
  #-}

module Monero.Wallet.Process where

import Data.Process
import Data.Json.RPC
import Monero.Wallet.RPC

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.IP (IPv4)
import Data.Default
import Data.Aeson.Lens (key)
import Control.Monad.Catch
import Control.Monad (void, unless, forM_, when, forever)
import Control.Concurrent (threadDelay, forkIO, forkFinally, ThreadId, killThread)
import Control.Concurrent.Async

import System.FilePath
import System.IO
import System.IO.Error (isEOFError, isDoesNotExistError)
import System.Exit (ExitCode (..))
import System.Directory (getCurrentDirectory, removeFile)
import System.Process (waitForProcess, interruptProcessGroupOf
                      , readCreateProcessWithExitCode, shell)
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#endif

import Network (HostName, PortNumber)
import Network.Wreq (get, responseBody)
import Control.Lens ((^?))






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
#ifdef mingw32_HOST_OS
          , moneroWalletCliPath = "monero-wallet-cli.exe"
#else
          , moneroWalletCliPath = "monero-wallet-cli"
#endif
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
  { makeWalletName     :: T.Text          -- ^ Names need to be unique
  , makeWalletPassword :: T.Text
  , makeWalletLanguage :: WalletLanguage
  , makeWalletSeed     :: Maybe T.Text    -- ^ Electrum-style seed
  , makeWalletInterval :: Int             -- ^ In picoseconds
  , makeWalletProgress :: Double -> IO () -- ^ Callback to invoke
  }

instance Show MakeWalletConfig where
  show MakeWalletConfig{..} =
       "MakeWalletConfig { makeWalletName = " ++ show makeWalletName
                     ++ ", makeWalletPassword = " ++ show makeWalletPassword
                     ++ ", makeWalletLanguage = " ++ show makeWalletLanguage
                     ++ ", makeWalletSeed = " ++ show makeWalletSeed
                     ++ ", makeWalletInterval = " ++ show makeWalletInterval
                     ++ ", makeWalletProgress = <function> } "

instance Eq MakeWalletConfig where
  (==)  (MakeWalletConfig
          { makeWalletName = n1
          , makeWalletPassword = p1
          , makeWalletLanguage = l1
          , makeWalletSeed = s1
          , makeWalletInterval = i1
          })
        (MakeWalletConfig
          { makeWalletName = n2
          , makeWalletPassword = p2
          , makeWalletLanguage = l2
          , makeWalletSeed = s2
          , makeWalletInterval = i2
          })
        = n1 == n2 && p1 == p2 && l1 == l2 && s1 == s2 && i1 == i2


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
             ] ++ case makeWalletSeed of
                    Nothing -> []
                    Just s  -> [ "--restore-deterministic-wallet"
                               , "--electrum-seed=" ++ T.unpack s
                               ]

  tryRemoveFile $ name' ++ ".log"
  tryRemoveFile $ name' ++ ".stdout.log"

  ProcessHandles{..} <- mkProcess moneroWalletCliPath args

  threadDelay (2 * second)
  T.hPutStrLn stdinHandle . T.pack . show $
    case makeWalletSeed of
      Nothing -> walletLanguageCode makeWalletLanguage
      Just _  -> 0 :: Int -- FIXME: blockchain height - Weird behavior
  hFlush stdinHandle

  errWatcher <- async $ throwLogging name'
  link errWatcher

  maxHeightResp <- get "http://moneroblocks.info/api/get_stats/"
  let maxHeight = maxHeightResp ^? responseBody . key "height"

  progressWatcher <- async $ forever $ do
    -- read last line of name' <.> "log"
    -- parse it, try and get the current block height so far
    -- invoke callback if value was parsed, and is 0 <= parsed/maxHeight <= 1
    threadDelay makeWalletInterval

  -- neglectFile (name' ++ ".log") second
  threadDelay $ 10 * second

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

  tryRemoveFile $ name' ++ ".log"
  tryRemoveFile $ name' ++ ".stdout.log"

  hs@ProcessHandles{stdoutHandle} <- mkProcess moneroWalletCliPath args

  stdLog <- hGetContents stdoutHandle
  stdLogger  <- forkIO $ writeFile (name' ++ ".stdout.log") stdLog
  errWatcher <- async $ throwLogging name'
  link errWatcher

  let loop = do
        threadDelay second
        xs <- T.lines <$> T.readFile (name' ++ ".log")
        unless (any ("Starting wallet rpc server" `T.isInfixOf`) xs) loop
  loop

  cfg <- newRPCConfig walletRpcIp walletRpcPort

  killThread stdLogger

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
  forM_ xs $ \x -> when ("ERROR" `T.isInfixOf` x) $ throwM $ LoggingError x


nextAvailPort :: PortNumber -> IO PortNumber
nextAvailPort = go
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
