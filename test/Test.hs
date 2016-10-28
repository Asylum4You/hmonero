{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where

import Monero.Wallet.RPC
import Monero.Wallet.Process
import Monero.Types ( PaymentId (..), HexString (..)
                    , Address (Address), Base58String (..)
                    , TransferDestination (..)
                    )
import Data.Process (stdoutHandle)

import Test.Tasty
import Test.Tasty.QuickCheck as Q
import Test.Tasty.HUnit
import Test.HUnit

import Data.Default
import qualified Data.Text.IO as T
import System.Directory (removeFile)
import System.IO (stdout)
import System.IO.Error (isEOFError)
import GHC.IO.Handle
import Control.Monad (void)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception

import Debug.Trace




main :: IO ()
main = do
  (cfg,hs) <- openWallet def OpenWalletConfig
                              { openWalletName     = "foo"
                              , openWalletPassword = "asdf"
                              , openWalletInterval = 1000000
                              , openWalletProgress = \r -> print r
                              }
  putStrLn "Opened foo wallet"
  bracket_ (pure ()) (closeWallet hs) $
    defaultMain $ testGroup "hmonero"
      [ testGroup "Wallet"
          [ testGroup "Process"
              [ testCase "makeWallet" $
                  bracket_
                    (pure ())
                    (mapM_ tryRemoveFile ["bar","bar.log","bar.address.txt","bar.keys"])
                    $ makeWallet def MakeWalletConfig
                                  { makeWalletName     = "bar"
                                  , makeWalletPassword = "asdf"
                                  , makeWalletLanguage = English
                                  , makeWalletSeed     = Nothing
                                  , makeWalletInterval = 1000000
                                  , makeWalletProgress = \r -> print r
                                  }
              , testCase "makeWallet mnemonic" $ do
                  threadDelay 1000000
                  mn <- T.readFile "bar.mnemonic"
                  makeWallet def MakeWalletConfig
                                  { makeWalletName     = "bar"
                                  , makeWalletPassword = "asdf"
                                  , makeWalletLanguage = English
                                  , makeWalletSeed     = Just mn
                                  , makeWalletInterval = 1000000
                                  , makeWalletProgress = \r -> print r
                                  }
              , testCase "openWallet closeWallet" $
                  bracket_
                    (pure ())
                    (mapM_ removeFile ["bar","bar.log","bar.address.txt","bar.keys"])
                    $ do  port <- nextAvailPort 18082
                          let walletProcessConf = def
                                { walletRpcPort = port }
                          (_,hsO) <- openWallet walletProcessConf
                                       OpenWalletConfig
                                         { openWalletName     = "bar"
                                         , openWalletPassword = "asdf"
                                         , openWalletInterval = 1000000
                                         , openWalletProgress = \r -> print r
                                         }
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
                  [ testCase "getPayments" $ void $ getPayments cfg $ GetPayments $ PaymentId $ HexString "792fbe5f113646e522f938b73a6f2043341740ba2f15be15cea8c4a742c845a7"
                  , testCase "getPayments integrated" $ void $ getPayments cfg $ GetPayments $ PaymentId $ HexString "6b14a380273d57ee"
                  , testCase "getBulkPayments" $ void $ getBulkPayments cfg $ GetBulkPayments [PaymentId $ HexString "792fbe5f113646e522f938b73a6f2043341740ba2f15be15cea8c4a742c845a7", PaymentId $ HexString "6b14a380273d57ee"] 1000000
                  , testCase "splitIntegratedAddress" $ void $ splitIntegratedAddress cfg $ SplitIntegratedAddress $ Address $ Base58String "4JH4YSpPdgQ1KVVuPUunjeVTm6zf6Vg3kf38VhSCunCzQjWBzH3rpJjSwDgnE8ocJWYGeEqhEoBWqeoVwbyYs3se9GRKNmpkarrTsn7uja"
                  ]
              , testGroup "Lossy"
                  [ testCase "transfer" $ void $ transfer cfg MakeTransfer
                      { makeTransferDestinations =
                          [ TransferDestination 1000000000 $ Address $ Base58String "48iZ4NPuYsTfZEiYYXzKbTeZotimqEsfUB2LgykPAksdHkz4daHT46ZFsnkwRygxu2KR3KmkhpLvNQMtszjC3TsVFMLSNwK"
                          ]
                      , makeTransferMixin        = 1
                      , makeTransferUnlockTime   = 0
                      , makeTransferPaymentId    = Nothing
                      , makeTransferGetTxKey     = False
                      }
                  , testCase "transferSplit" $ void $ transferSplit cfg MakeTransferSplit
                      { makeTransferSplitDestinations =
                          [ TransferDestination 1000000000 $ Address $ Base58String "48iZ4NPuYsTfZEiYYXzKbTeZotimqEsfUB2LgykPAksdHkz4daHT46ZFsnkwRygxu2KR3KmkhpLvNQMtszjC3TsVFMLSNwK"
                          ]
                      , makeTransferSplitMixin        = 3
                      , makeTransferSplitUnlockTime   = 0
                      , makeTransferSplitPaymentId    = Nothing
                      , makeTransferSplitGetTxKey     = False
                      , makeTransferSplitNewAlgorithm = True
                      }
                  , testCase "stopWallet" $ void $ stopWallet cfg
                  ]
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
