{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  #-}

module Data.Process where

import System.Process
import GHC.Generics
import GHC.IO.Handle (Handle)
import qualified Data.Text as T
import Data.Attoparsec.Text as A
import Data.IP (IPv4)
import Data.List (intercalate)
import Network (PortNumber)
import Control.Applicative
import Control.Monad.Catch
import Control.Monad (void)



-- * Processes

data ProcessHandles = ProcessHandles
  { stdinHandle   :: Handle
  , stdoutHandle  :: Handle
  , stderrHandle  :: Handle
  , processHandle :: ProcessHandle
  }


mkProcess :: FilePath -> [String] -> IO ProcessHandles
mkProcess cmd args = do
  let p = proc cmd args
  r <- createProcess p
        { std_in       = CreatePipe
        , std_out      = CreatePipe
        , std_err      = CreatePipe
        , create_group = True
        }
  case r of
    (Just sIn, Just sOut, Just sErr, h) ->
      pure ProcessHandles
        { stdinHandle   = sIn
        , stdoutHandle  = sOut
        , stderrHandle  = sErr
        , processHandle = h
        }
    _ ->
      throwM NotEnoughHandles
        { notEnoughHandlesCmd  = cmd
        , notEnoughHandlesArgs = [] -- args
        }


-- * Exceptions


data MakeProcessException
  = NotEnoughHandles
      { notEnoughHandlesCmd  :: FilePath
      , notEnoughHandlesArgs :: [String]
      }
  | StdoutBlockTimeout
      { stdoutBlockTimeoutHandle :: Handle
      }
  deriving (Show, Eq, Generic)
instance Exception MakeProcessException


data CloseProcessException
  = NonZeroExitCode Int
  deriving (Show, Eq, Generic)
instance Exception CloseProcessException


-- * Feedback

getCmd :: T.Text -> T.Text
getCmd x = let (_:_:xs) = T.words x in T.unwords xs


data Feedback
  = Header
  | LogLevel Int
  | DefaultLogFile
  | LoggingTo Int FilePath
  | LoadingWallet
  | LoadedWalletKeysFile
  | LoadedOk
  | BindingOn IPv4 PortNumber
  | StartingRPCServer
  | RunNetService



parseFeedback :: Parser Feedback
parseFeedback =  parseHeader
             <|> parseLogLevel
             <|> parseDefaultLogFile
             <|> parseLoggingTo
             <|> parseLoadingWallet
             <|> parseLoadedWalletKeysFile
             <|> parseLoadedOk
             <|> parseBindingOn
             <|> parseStartingRPCServer
             <|> parseRunNetService
  where
    parseHeader =
      Header <$ A.string "Monero 'Wolfram Warptangent' (v0.10.0.0-release)"
    parseLogLevel =
      (LogLevel . round) <$> (A.string "Setting log level = " >> A.double)
    parseDefaultLogFile =
      DefaultLogFile <$ A.string "default_log: monero-wallet-cli.log"
    parseLoggingTo =
      LoggingTo <$> (A.string "Logging at log level " >> (round <$> A.double))
                <*> (A.string " to " >> many1 A.anyChar <* endOfLine)
    parseLoadingWallet =
      LoadingWallet <$ A.string "Loading wallet..."
    parseLoadedWalletKeysFile =
      LoadedWalletKeysFile <$ A.string "Loaded wallet keys file, with public address: "
    parseLoadedOk =
      LoadedOk <$ A.string "Loaded ok"
    parseBindingOn = do
      ip <- A.string "Binding on "
         >> many1 A.digit `sepBy1` A.char '.' :: Parser [String]
      void $ char ':'
      port <- many1 A.digit
      pure $ BindingOn (read $ intercalate "." ip) (fromInteger $ read port)
    parseStartingRPCServer =
      StartingRPCServer <$ A.string "Starting wallet rpc server"
    parseRunNetService =
      RunNetService <$ A.string "Run net_service loop( 1 threads)..."
