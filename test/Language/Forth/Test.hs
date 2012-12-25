{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Monad                        (zipWithM)

import           Data.Bits                            (complement, xor, (.&.))
import           Data.Functor                         ((<$>))

import           Language.Forth.Instructions
import           Language.Forth.Interpreter
import           Language.Forth.Parse
import           Language.Forth.Run
import           Language.Forth.Stack
import           Language.Forth.State
import           Language.Forth.Synthesis

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

-- Testing the utility functions for actually synthesizing programs:
case_toNative1 = parseProgram "@p . @p . 2 10 or . . ." @=?
                 toNative [Number 2, Opcode Nop, Number 10, Opcode Or]
case_toNative2 = parseProgram "@p . @p + 2 10" @=?
                 toNative [Number 2, Opcode Nop, Number 10, Opcode Plus]


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
case_5 = do let res = runProgram "dup or a! @p 123 !+ @p ! . 456 dup or a! . @+ 2* @+ . 2/ + ! ."
            a res @=? 2
            memory res ! 0 @=? 123
            memory res ! 1 @=? 456
            memory res ! 2 @=? 474
            p res @=? 7
            unchanged [b, r, s, t] res
case_ret = do let res = runProgram "call 2 . . . . ; . . ."
              p res @=? 1
              unchanged [a, b, r, s, t] res
case_jump = do let res = runProgram "jump 42"
               p res @=? 42
               unchanged [a, b, r, s, t] res
case_call = do let res = runProgram "call 10"
               r res @=? 1
               p res @=? 10
               unchanged [a, b, s, t] res
case_unext = do let res = runProgram ". . unext ."
                p res @=? 1
                unchanged [a, b, r, s, t] res
case_unext' = do let res = runProgram "@p push . . 41 @+ . . unext"
                 p res @=? 3
                 a res @=? 42
                 unchanged [b, r, s, t] res
case_if = do let res = runProgram "if 42"
             p res @=? 1
             unchanged [a, b, r, s, t] res
case_if' = do let res = runProgram "@p if 42 10"
              p res @=? 42
              unchanged [a, b, r, s] res
case_minusIf = do let res = runProgram "-if 42"
                  p res @=? 42
                  unchanged [a, b, r, s, t] res
case_minusIf' = do let res = runProgram "- -if 42"
                   p res @=? 1
                   unchanged [a, b, r, s] res
case_fetchP = do let res = runProgram "@p . . . 42"
                 p res @=? 2
                 t res @=? 42
                 unchanged [a, b, r, s] res
case_fetchPlus = do let res = runProgram "@+ . . ."
                    a res @=? 1
                    t res @=? memory res ! 0
                    unchanged [b, r, s] res
case_fetchB = do let res = runProgram "@b . . ."
                 t res @=? memory res ! 0
                 unchanged [b, r, s] res
case_fetch = do let res = runProgram "@ . . ."
                t res @=? memory res ! 0
                unchanged [b, r, s] res
case_storeP = do let res = runProgram "@p !p . . 42"
                 p res @=? 3
                 memory res ! (p res - 1) @=? 42
                 unchanged [a, b, r, s] res
case_storePlus = do let res = runProgram "@p !+ . . 42"
                    a res @=? 1
                    p res @=? 2
                    memory res ! 0 @=? 42
                    unchanged [b, r, s] res
case_storePlus' = do let res = runProgram "@p @p a! . 42 10 !+ . . ."
                     a res @=? 11
                     p res @=? 4
                     memory res ! 10 @=? 42
                     unchanged [b, r, s] res
case_storePlus'' = do let res = runProgram "dup or a! @p 123 !+ @p ! . 456"
                      a res @=? 1
                      memory res ! 0 @=? 123
                      memory res ! 1 @=? 456
                      unchanged [b, r, s, t] res
case_storeB = do let res = runProgram "@p !b . . 42"
                 p res @=? 2
                 memory res ! 0 @=? 42
                 unchanged [a, b, r, s] res
case_storeB' = do let res = runProgram "@p @p b! . 42 10 !b . . ."
                  p res @=? 4
                  memory res ! 10 @=? 42
                  b res @=? 10
                  unchanged [a, r, s] res
case_store = do let res = runProgram "@p ! . . 42"
                p res @=? 2
                memory res ! 0 @=? 42
                unchanged [a, b, r, s] res
case_store' = do let res = runProgram "@p @p a! . 42 10 ! . . ."
                 a res @=? 10
                 p res @=? 4
                 memory res ! 10 @=? 42
                 unchanged [b, r, s] res
case_store'' = do let res = runProgram "dup or a! @p 123 ! . . ."
                  a res @=? 0
                  memory res ! 0 @=? 123
                  unchanged [b, r, s, t] res
case_times2 = do let res = runProgram "@p 2* . . 2"
                 t res @=? 4
                 p res @=? 2
                 unchanged [a, b, r, s] res
case_div2 = do let res = runProgram "@p 2/ . . 4"
               t res @=? 2
               p res @=? 2
               unchanged [a, b, r, s] res
case_not = do let res = runProgram "- . . ."
              t res @=? (- 1)
              p res @=? 1
              unchanged [a, b, r, s] res
case_not' = do let res = runProgram "@p - . . 42"
               t res @=? complement 42
               p res @=? 2
               unchanged [a, b, r, s] res
case_plus = do let res = runProgram "@p @p . + 12 30"
               t res @=? 42
               p res @=? 3
               unchanged [a, b, r, s] res
case_and = do let res = runProgram "@p @p and . 12 30"
              t res @=? 12 .&. 30
              p res @=? 3
              unchanged [a, b, r, s] res
case_or = do let res = runProgram "@p @p or . 12 30"
             t res @=? 12 `xor` 30
             p res @=? 3
             unchanged [a, b, r, s] res
case_drop = do let res = runProgram "@p @p drop . 1 2"
               t res @=? 1
               p res @=? 3
               unchanged [a, b, r, s] res
case_dup = do let res = runProgram "@p dup . . 42"
              t res @=? 42
              s res @=? 42
              p res @=? 2
              unchanged [a, b, r] res
case_dup' = do let res = runProgram "@p dup or . 42"
               t res @=? 0
               p res @=? 2
               unchanged [a, b, r, s] res
case_pop = do let res = runProgram "call 2 0 pop . . ."
              t res @=? 1
              unchanged [a, b, r, s] res
case_over = do let res = runProgram "@p @p over . 1 2"
               t res @=? 1
               s res @=? 2
               p res @=? 3
               unchanged [a, b, r] res
case_a = do let res = runProgram "@p a! a . 42"
            a res @=? 42
            t res @=? 42
            p res @=? 2
            unchanged [b, r, s] res
case_nop = do let res = step $ runProgram ". . . ."
              p res @=? 1
              unchanged [a, b, r, s, t] res
case_push = do let res = runProgram "@p push . . 42"
               r res @=? 42
               p res @=? 2
               unchanged [a, b, s, t] res
case_setB = do let res = runProgram "@p b! . . 42"
               b res @=? 42
               p res @=? 2
               unchanged [a, r, s, t] res
case_setA = do let res = runProgram "@p a! . . 42"
               a res @=? 42
               p res @=? 2
               unchanged [b, r, s, t] res
