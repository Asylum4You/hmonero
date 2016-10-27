{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  , CPP
  , DeriveGeneric
  #-}

module Monero.Wallet.Process where

import Data.Process
import Data.Json.RPC
import Monero.Wallet.RPC

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.IP (IPv4)
import Data.Default
import Data.Aeson as Aeson
import Data.Aeson.Lens (key)
import Data.Attoparsec.Text as A
import Data.Scientific (toRealFloat)
import Data.Word (Word8)
import Control.Monad.Catch
import Control.Monad (void, unless, forM_, when, forever, replicateM)
import Control.Applicative
import Control.Concurrent (threadDelay, forkIO, forkFinally, ThreadId, myThreadId, killThread)
import Control.Concurrent.Async
import Control.Lens ((^?))

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

import GHC.Generics

import Debug.Trace



for = flip map



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
             , "--log-file="            ++ name' <.> "log"
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

  tryRemoveFile $ name' <.> "log"
  tryRemoveFile $ name' <.> "stdout.log"

  ProcessHandles{..} <- mkProcess moneroWalletCliPath args

  threadDelay (2 * second)
  T.hPutStrLn stdinHandle . T.pack . show $
    case makeWalletSeed of
      Nothing -> walletLanguageCode makeWalletLanguage
      Just _  -> 0 :: Int -- FIXME: blockchain height - Weird behavior
  hFlush stdinHandle

  maxHeightResp <- get "http://moneroblocks.info/api/get_stats/"
  let mMaxHeight = maxHeightResp ^? responseBody . key "height"

  maxHeight <- case mMaxHeight of
    Nothing -> error "moneroblocks.info not reachable"
    Just x@(Aeson.String h) -> case A.parseOnly (A.many1 A.digit) h of
      Left _ -> error $ "moneroblocks.info responded with unexpected data: " ++ show x
      Right h' -> pure $ read h' :: IO Int
    Just x -> error $ "moneroblocks.info responded with unexpected data: " ++ show x

  progressWatcher <- async $ forever $ do
    (_,mH) <- parseLogLines name'
    case mH of
      Nothing -> pure ()
      Just h  -> do
        let r = fromIntegral h / fromIntegral maxHeight
        makeWalletProgress $
          if r < 0
          then 0
          else if r > 1
          then 1
          else r
        when (r >= 0.95) $ do
          self <- myThreadId
          killThread self
    threadDelay makeWalletInterval
  link progressWatcher

  -- neglectFile (name' ++ ".log") second
  threadDelay $ 5 * second

  interruptProcessGroupOf processHandle
  mapM_ hClose [stdinHandle, stdoutHandle, stderrHandle]


-- * Opening Existing Wallet

data OpenWalletConfig = OpenWalletConfig
  { openWalletName     :: T.Text
  , openWalletPassword :: T.Text
  , openWalletInterval :: Int             -- ^ In picoseconds
  , openWalletProgress :: Double -> IO () -- ^ Callback to invoke
  }

data Continue = Continue deriving (Show, Generic)
instance Exception Continue


openWallet :: WalletProcessConfig
           -> OpenWalletConfig
           -> IO (RPCConfig, ProcessHandles)
openWallet WalletProcessConfig{..} OpenWalletConfig{..} = do
  dir <- if walletsDir == "."
         then getCurrentDirectory
         else pure walletsDir
  let name' = dir </> T.unpack openWalletName
      args = [ "--wallet-file="   ++ name'
             , "--log-file="      ++ name' <.> "log"
             , "--log-level=2"
             , "--password="      ++ T.unpack openWalletPassword
             , "--rpc-bind-ip="   ++ show walletRpcIp
             , "--rpc-bind-port=" ++ show (fromIntegral walletRpcPort :: Int)
             , "--daemon-host="   ++ walletDaemonHost
             , "--daemon-port="   ++ show (fromIntegral walletDaemonPort :: Int)
             ]

  tryRemoveFile $ name' <.> "log"
  tryRemoveFile $ name' <.> "stdout.log"

  hs@ProcessHandles{stdoutHandle} <- mkProcess moneroWalletCliPath args

  stdLog <- hGetContents stdoutHandle
  stdLogger  <- async $ writeFile (name' <.> "stdout.log") stdLog
  link stdLogger


  maxHeightResp <- get "http://moneroblocks.info/api/get_stats/"
  let mMaxHeight = maxHeightResp ^? responseBody . key "height"

  maxHeight <- case mMaxHeight of
    Nothing -> error "moneroblocks.info not reachable"
    Just x@(Aeson.String h) -> case A.parseOnly (A.many1 A.digit) h of
      Left _ -> error $ "moneroblocks.info responded with unexpected data: " ++ show x
      Right h' -> pure $ read h' :: IO Int
    Just x -> error $ "moneroblocks.info responded with unexpected data: " ++ show x

  progressWatcher <- async $
    let x = forever $ do
              (rpcStarted, mH) <- parseLogLines name'
              if rpcStarted
              then throwM Continue
              else case mH of
                Nothing -> pure ()
                Just h  -> openWalletProgress $
                            let r = fromIntegral h / fromIntegral maxHeight
                            in  if r < 0 then 0 else if r > 1 then 1 else r
              threadDelay openWalletInterval
    in  x `catch` (\Continue -> pure ())
  link progressWatcher

  putStrLn "JSON RPC server started"

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

data WalletLogFileLine
  = WalletLogFileError String
  | WalletLogFileHeight Int
  | WalletLogFileOther

instance Monoid WalletLogFileLine where
  mempty = WalletLogFileOther
  x `mappend` y =
    case (x,y) of
      (WalletLogFileHeight hX, WalletLogFileHeight hY) ->
        WalletLogFileHeight $ max hX hY
      (WalletLogFileError eX, WalletLogFileError eY) ->
        WalletLogFileError $ unlines [eX, eY]
      (WalletLogFileError _, _) -> x
      (_, WalletLogFileError _) -> y
      (WalletLogFileOther,_) -> y
      (_,WalletLogFileOther) -> x



tryRemoveFile :: FilePath -> IO ()
tryRemoveFile file =
  removeFile file `catch` handleError
  where
    handleError e | isDoesNotExistError e = pure ()
                  | otherwise             = throwM e


parseLogLines :: FilePath -> IO (Bool, Maybe Int)
parseLogLines name' = do
  xs <- T.lines <$> T.readFile (name' <.> "log")
  let (rpcStarted, ys) = unzip $ for xs $ \x ->
        if "ERROR" `T.isInfixOf` x
        then (False, WalletLogFileError $ show x)
        else if "Starting wallet rpc server" `T.isInfixOf` x
        then (True, WalletLogFileOther)
        else case A.parseOnly lineParser $ T.drop 28 x of
              Left e ->
                          traceShow (e, T.drop 28 x)
                            (False, WalletLogFileOther)
              Right (y@(WalletLogFileHeight _)) -> (False, y)
              _ -> error $ "Somehow parsed something not possible :| "
                          ++ show x
                          ++ " :: all lines :| "
                          ++ show xs
  case mconcat ys of
    WalletLogFileOther    -> pure (any id rpcStarted, Nothing)
    WalletLogFileError e  -> throwM $ LoggingError $ T.pack e
    WalletLogFileHeight h -> pure (any id rpcStarted, Just h)
  where
    lineParser :: A.Parser WalletLogFileLine
    lineParser = do
      parseHeight1 <|> parseHeight3 <|> parseHeight2
      where
        parseHeight1 :: A.Parser WalletLogFileLine
        parseHeight1 = do
          _ <- A.string "Skipped block by height: " <?> "skipping block by height"

          cs <- A.many1 A.digit <?> "height"

          A.endOfLine

          pure $ WalletLogFileHeight $ read cs

        parseHeight2 :: A.Parser WalletLogFileLine
        parseHeight2 = do
          _ <- A.string "Processed block: <" <?> "processed block"
          void $ replicateM 64 (A.hexadecimal :: A.Parser Word8)
          _ <- A.string ">, height " <?> "end processed block"

          cs <- A.many1 A.digit <?> "height"
          void $ A.char ','

          _ <- A.many1 A.anyChar <?> "rest of processed block"

          A.endOfLine

          pure $ WalletLogFileHeight $ read cs

        parseHeight3 :: A.Parser WalletLogFileLine
        parseHeight3 = do
          _ <- A.string "Skipped block by timestamp, height: " <?> "skipping block by height"

          cs <- A.many1 A.digit <?> "height"

          void $ A.char ','

          _ <- A.many1 A.anyChar <?> "rest of skipped timestamp"

          A.endOfLine

          pure $ WalletLogFileHeight $ read cs


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
