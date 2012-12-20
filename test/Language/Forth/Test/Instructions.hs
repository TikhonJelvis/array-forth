{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Data.Functor                         ((<$>))

import           Language.Forth.Instructions

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.QuickCheck                      ((==>))
import           Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary)

instance Arbitrary F18Word where arbitrary = fromInteger <$> arbitrary

main = $(defaultMainGenerator)

prop_bits word = word == (toBits $ fromBits word)

prop_opcode word = word < 0x20 ==> word == (fromOpcode $ toOpcode word)
