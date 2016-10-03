{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where

import Monero.Wallet.RPC
import Monero.Wallet.Process
import Data.Process (stdoutHandle)

import Test.Tasty
import Test.Tasty.QuickCheck as Q
import Test.Tasty.HUnit
import Test.HUnit

import Data.Default
import System.Directory (removeFile)
import System.IO (stdout)
import System.IO.Error (isEOFError)
import GHC.IO.Handle
import Control.Monad (void)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception




main :: IO ()
main = do
  (cfg,hs) <- openWallet def $ OpenWalletConfig
                { openWalletName     = "foo"
                , openWalletPassword = "asdf"
                }
  forkIO $ loggingTo "foo.test.log" $ stdoutHandle hs
  bracket_ (pure ()) (closeWallet hs) $
    defaultMain $ testGroup "hmonero"
      [ testGroup "Wallet"
          [ testGroup "Process"
              [ testCase "makeWallet" $
                  makeWallet def $
                    MakeWalletConfig
                      { makeWalletName     = "bar"
                      , makeWalletPassword = "asdf"
                      , makeWalletLanguage = English
                      }
              , testCase "openWallet closeWallet" $
                  bracket_
                    (pure ())
                    (mapM_ removeFile ["bar","bar.log","bar.address.txt","bar.keys"])
                    $ do  (_,hsO) <- openWallet def $ OpenWalletConfig
                            { openWalletName     = "bar"
                            , openWalletPassword = "asdf"
                            }
                          forkIO $ loggingTo "bar.test.log" $ stdoutHandle hsO
                          threadDelay (5 * second) -- FIXME
                          closeWallet hsO
              ]
          , testGroup "RPC"
              [ testGroup "Stateless"
                  [ testCase "getBalance" $ void $ getBalance cfg
                  , testCase "getAddress" $ void $ getAddress cfg
                  , testCase "getHeight"  $ void $ getHeight cfg
                  , testCase "sweepDust"  $ void $ sweepDust cfg
                  , testCase "queryKey"   $ void $ queryKey cfg $ QueryKey KeyMnemonic
                  , testCase "incomingTransfers" $
                      void $ incomingTransfers cfg $ GetIncomingTransfers AllTransferTypes
                  , testCase "makeIntegratedAddress" $
                      void $ makeIntegratedAddress cfg $ MakeIntegratedAddress Nothing
                  , testCase "store" $ void $ store cfg
                  ]
              , testGroup "Predicated"
                  [] -- getPayments getBulkPayments
                     -- testCase "splitIntegratedAddress" $ splitIntegratedAddress cfg
              , testGroup "Stateful"
                  [] -- transfer transferSplit
                     -- stopWallet
              ]
          ]
      ]



loggingTo :: String -> Handle -> IO ()
loggingTo file h =
  let loop = do
        xs <- hGetContents h
        writeFile file xs
        -- loop
  in  loop `catch` handleE
  where
    handleE e | isEOFError e = pure ()
              | otherwise    = throw e
