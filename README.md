# hmonero

This is a very simple haskell-accessible wrapper over the `monero-wallet-cli`
executable (and in the future, the `monderod` executable) for the
[MoneyBit monero client](http://moneybit.science).


## Building

0. Verify `monero-wallet-cli` is in your `$PATH`
1. Get [stack](https://haskellstack.org)
2. `stack build`


## Usage

This wrapper simply forks child processes and interacts with them with
the JSON-RPC HTTP api. To open a wallet, see the `Monero.Wallet.Process`
module, and to invoke the api endpoints, see the `Monero.Wallet.RPC` module.

You can build the HTML documentation for this package by running `stack haddock`,
building to `.stack-work/dist/*/*/doc/html/hmonero/index.html`.


## How to run tests

The tests expect a simple `foo` wallet to reside in the root
directory, specifically using the password `asdf` verbatim.
The seed for the `foo` wallet is:

```
eternal digit injury popular vampire austere seismic cottage
juggled upon annoyed summon deity catch dullness nuns
object fountain code knapsack gifts inactive abnormal anxiety upon
```

There's a few transactions in its history that are hardcoded in the
unit tests, so we're leaving this wallet open. Feel free to donate or...
steal the funds :) I know how it goes.

To run the tests, run `stack test`.
