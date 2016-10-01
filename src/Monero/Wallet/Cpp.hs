{-# LANGUAGE
    CPP
  , ForeignFunctionInterface
  #-}

module Monero.Wallet.Cpp where

import Foreign
import Foreign.C


#include "monero2.h"
