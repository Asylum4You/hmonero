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

- Process monad, where txns can only happen in an open process
- exit closes handles & kills process
- codify actions

data WalletQuery
  = Address
  | Balance
  | Payments [PaymentId]
  | TxInfo TransactionId

data WalletSign
  = Sign FilePath
  | Verify FilePath

data WalletAuthQuery
  = Seed
  | SpendKey
  | ViewKey

data WalletUpdate
  = Transfer
      { transferMixins :: Int
      , payees :: [(Address, Double)]
      }

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
