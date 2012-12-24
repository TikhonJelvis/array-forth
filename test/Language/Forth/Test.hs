{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Monad                        (zipWithM)

import           Data.Functor                         ((<$>))

import           Language.Forth.Instructions
import           Language.Forth.Run
import           Language.Forth.Stack
import           Language.Forth.State

import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.HUnit
import           Test.QuickCheck                      ((==>))
import           Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary)

instance Arbitrary F18Word where arbitrary = fromInteger <$> arbitrary

instance Arbitrary Stack where arbitrary = foldl push empty <$> arbitrary

main = $(defaultMainGenerator)

-- Instruction utilities tests:
prop_bits word = word == (toBits $ fromBits word)
prop_opcode word = word < 0x20 ==> word == (fromOpcode $ toOpcode word)
prop_pushPop word stack = word == snd (pop $ push stack word)
prop_pop stack = stack == foldl1 (.) (replicate 8 $ fst . pop) stack

-- Interpreter tests (ported from Racket):
unchanged regs state = assertBool "Something changed!" . and $ zipWith (==) start new
  where start = map ($ startState) regs
        new   = map ($ state) regs
case_1 = do let res = runProgram "@p @p + . 2 3"
            p res @=? 3
            t res @=? 5
            unchanged [a, b, r, s] res
case_2 = do let res = runProgram "@p - . . 0"
            p res @=? 2
            t res @=? (- 1)
            unchanged [a, b, r, s] res
case_3 = do let res = runProgram "@p b! @p . 4 42 !b @p . ."
            p res @=? 5
            t res @=? 42
            b res @=? 4
            memory res ! 4 @=? 42
            unchanged [a, r, s] res
case_4 = do let res = runProgram "- dup dup dup dup dup dup dup"
            p res @=? 2
            t res @=? (- 1)
            s res @=? (- 1)
            dataStack res @=? fill empty [0, 0, -1, -1, -1, -1, -1, -1]
            unchanged [a, b, r] res