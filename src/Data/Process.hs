{-# LANGUAGE
    DeriveGeneric
  #-}

module Data.Process where

import Control.Monad.Catch
import System.Process
import GHC.Generics
import GHC.IO.Handle (Handle)


-- * Processes

data ProcessHandles = ProcessHandles
  { stdinHandle  :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  }


mkProcess :: FilePath -> [String] -> IO ProcessHandles
mkProcess cmd args = do
  let p = proc cmd args
  r <- createProcess p
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  case r of
    (Just sIn, Just sOut, Just sErr, _) ->
      pure ProcessHandles
        { stdinHandle  = sIn
        , stdoutHandle = sOut
        , stderrHandle = sErr
        }
    _ -> throwM $ NotEnoughHandles
           { notEnoughHandlesCmd  = cmd
           , notEnoughHandlesArgs = args
           }


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