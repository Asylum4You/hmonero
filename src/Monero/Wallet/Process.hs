{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  #-}

module Monero.Wallet.Process where

import Data.Process

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Catch

import System.FilePath
import System.IO
import System.IO.Error (isEOFError)
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)



-- | Global config options for all wallets
data WalletProcessConfig = WalletProcessConfig
  { walletsDir          :: FilePath -- ^ Directory where wallets are stored
  , moneroWalletCliPath :: FilePath -- ^ Executable path for `monero-wallet-cli`
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


data MakeWalletConfig = MakeWalletConfig
  { walletName     :: T.Text -- ^ Names need to be unique
  , walletPassword :: T.Text
  , walletLanguage :: WalletLanguage
  }


makeWallet :: WalletProcessConfig
           -> MakeWalletConfig
           -> IO ()
makeWallet WalletProcessConfig{..} MakeWalletConfig{..} = do
  let name' = walletsDir </> T.unpack walletName
      args = [ "--generate-new-wallet=" ++ name'
             , "--log-file=" ++ name' ++ ".log"
             ]
  ProcessHandles{..} <- mkProcess name' args

  waitFor (== "password: ") stdoutHandle
  T.hPutStrLn stdinHandle walletPassword
  waitFor (== "password: ") stdoutHandle
  T.hPutStrLn stdinHandle walletPassword
  waitFor (== "Enter the number corresponding to the language of your choice: ")
          stdoutHandle
  T.hPutStrLn stdinHandle . T.pack . show $ walletLanguageCode walletLanguage
  waitFor ("[wallet " `T.isPrefixOf`) stdoutHandle
  T.hPutStrLn stdinHandle "exit"

  mapM_ hClose [stdinHandle, stdoutHandle, stderrHandle]


-- | blocks until the prompt is available
waitFor :: (T.Text -> Bool) -> Handle -> IO ()
waitFor isS h = do
  let second = 1000000

  r <- timeout (60 * second) $ do
    threadDelay second
    ml <- getLastLine h
    case ml of
      Just l | isS l -> pure ()
      _              -> waitFor isS h

  case r of
    Nothing -> throwM StdoutBlockTimeout
                 { stdoutBlockTimeoutHandle = h
                 }
    Just () -> pure ()


getLastLine :: Handle -> IO (Maybe T.Text)
getLastLine h =
  go Nothing False
  where
    go soFar True = pure soFar
    go soFar False = do
      (nextLine,goOn) <- (getLn <$> T.hGetLine h)
                  `catch` (\e -> if isEOFError e
                                 then pure (soFar,True)
                                 else throwM e)
      go nextLine goOn
      where
        getLn l = (Just l, False)
