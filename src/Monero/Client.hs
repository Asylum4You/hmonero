{-# LANGUAGE
    FlexibleContexts
  , NamedFieldPuns
  #-}

module Process.Client where

import Process.Types
import Config
import System.Process
import Application.Types

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Catch


-- TODO Codify:
{-

* Every wallet has a wallet file = ~/.moneybit/?
* Each wallet has a password - creating one should have passwords
  pre-inquired?
  - opening one should be done with --password

# maintain as much as possible in CLI args - piping is a bizzotch

-}



{-

TODO:

Monero.Client.Process

creating a wallet should be a oneshot execution of simplewallet; i.e.
run with --generate-wallet-file foo etc, enter in the chosen password via
stdin, then just exit.

Opening a wallet is where all the real work needs to be done - opened process
monad?

-}



-- TODO: new wallet vs. opening an old one (expectations?)
openSimpleWallet :: MonadApp m => String -> m Process
openSimpleWallet name = do -- FIXME: Bracket with `exit`
  wrkDir <- envWrkDir <$> ask
  Mutable{config} <- get
  let wallet = walletConfig config
      client = walletCliPath wallet
      monerod = walletMoneroDNode wallet

      args =
        let as = case monerod of
                    LocalNode -> []
                    RemoteNode r -> ["--daemon-host", r]
        in  [ "--log-file=" ++ wrkDir ++ "/wallets/" ++ name ++ ".log"
              -- FIXME Windows compat FIXME check & create
            , "--wallet-file=" ++ wrkDir ++ "/wallets/" ++ name
              -- FIXME Windows compat
            ]

      p = proc client args -- FIXME: Wallet file location
  r <- liftIO $ createProcess p
         { std_in = CreatePipe
         , std_out = CreatePipe
         , std_err = CreatePipe
         }
  case r of
    (Just sIn, Just sOut, Just sErr, _) ->
      pure Process
             { processName = client
             , stdinHandle = sIn
             , stdoutHandle = sOut
             , stderrHandle = sErr
             }
    _ -> throwM $ NotEnoughHandles client
