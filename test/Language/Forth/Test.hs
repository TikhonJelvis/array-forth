{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Applicative                  ((<$>), (<*>))
import           Control.Monad                        (zipWithM)

import           Data.Bits                            (complement, xor, (.&.))
import           Data.List                            (genericLength)

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
import           Test.QuickCheck                      (forAll, (==>))
import           Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen                  (Gen, elements, listOf,
                                                       oneof)

instance Arbitrary F18Word where arbitrary = fromInteger <$> arbitrary

wordBits bits = (((2 ^ bits) - 1) .&.) <$> arbitrary

instance Arbitrary Stack where arbitrary = foldl push empty <$> arbitrary

instance Arbitrary Opcode where arbitrary = elements opcodes

straight, jumps, fast, slow, inSlot3 :: Gen Opcode
straight = elements $ filter (not . isJump) opcodes
jumps    = elements $ filter isJump opcodes
fast     = elements $ filter (\ e -> opcodeTime e == 1.5) opcodes
slow     = elements $ filter (\ e -> opcodeTime e == 5) opcodes
inSlot3  = elements $ filter slot3 opcodes

instance Arbitrary Instrs where arbitrary = oneof [instrs, jump3, jump2, jump1, constant]

instrs, jump3, jump2, jump1, constant :: Gen Instrs
instrs = Instrs <$> straight <*> straight <*> straight <*> inSlot3
jump3 = Jump3 <$> straight <*> straight <*> jumps <*> wordBits 3
jump2 = Jump2 <$> straight <*> jumps <*> wordBits 8
jump1 = Jump1 <$> jumps <*> wordBits 10
constant = Constant <$> arbitrary

instance Arbitrary Instruction where
  arbitrary = oneof [opcode, number, unused]

opcode, number, unused :: Gen Instruction
opcode = Opcode <$> arbitrary
number = Number <$> arbitrary
unused = return Unused

straightlineProgram :: Gen Program
straightlineProgram = listOf $ oneof [Opcode <$> straight, number, unused]

main = $(defaultMainGenerator)

run = runProgram startState

-- Instruction utilities tests:
prop_bits word = word == (toBits $ fromBits word)
prop_opcode word = word < 0x20 ==> word == (fromOpcode $ toOpcode word)
prop_pushPop word stack = word == snd (pop $ push stack word)
prop_pop stack = stack == foldl1 (.) (replicate 8 $ fst . pop) stack
prop_runningTimeConstant program  = forAll constant $ \ c ->
  runningTime (program ++ [c]) == runningTime program

prop_evaluateRunningTime program = negate (evaluate program) == runningTime (toNative program)

prop_displayReadProgram program = program == parseProgram (displayProgram program)

-- Returns whether the given instruction word has jump addresses for
-- all the jumps and has no jumps without addresses.
isValid :: Instrs -> Bool
isValid (Instrs a b c d)   = all (not . isJump) [a, b, c] && slot3 d 
isValid (Jump3 a b c addr) = all (not . isJump) [a, b] && isJump c   
isValid (Jump2 a b addr)   = not (isJump a) && isJump b              
isValid (Jump1 a addr)     = isJump a                             
isValid Constant{}         = True 

-- For now, we do not really support jumps in the Program type.
prop_validNative = forAll straightlineProgram $ \ p -> all isValid $ toNative p

case_runningTime = do let time = runningTime . parseProgram
                      15.5 @=? time ". . . . @p . . . 10"
                      6    @=? time ". . . ."
                      20   @=? time "@p @p @p @p 1 2 3 4"

-- Testing the utility functions for actually synthesizing programs:
case_toNative1 = parseProgram "@p . @p . 2 10 or . . ." @=?
                 toNative [Number 2, Opcode Nop, Number 10, Opcode Or]
case_toNative2 = parseProgram "@p . @p + 2 10" @=?
                 toNative [Number 2, Opcode Nop, Number 10, Opcode Plus]

-- Interpreter tests (ported from Racket):
unchanged regs = assertBool "Something changed!" . and $ zipWith (==) start new
  where start = map ($ startState) regs
        new   = map ($ ?res) regs
case_1 = do let ?res = run "@p @p . + 2 3"
            3 @=? p ?res
            5 @=? t ?res
            unchanged [a, b, r, s]
case_2 = do let ?res = run "@p - . . 0"
            2 @=? p ?res
            (- 1) @=? t ?res
            unchanged [a, b, r, s]
case_3 = do let ?res = run "@p b! @p . 4 42 !b @p . ."
            5 @=? p ?res
            42 @=? t ?res
            4 @=? b ?res
            42 @=? memory ?res ! 4
            unchanged [a, r, s]
case_4 = do let ?res = run "- dup dup dup dup dup dup dup"
            2 @=? p ?res
            (- 1) @=? t ?res
            (- 1) @=? s ?res
            fill empty [0, 0, -1, -1, -1, -1, -1, -1] @=? dataStack ?res
            unchanged [a, b, r]
case_5 = do let ?res = run "dup or a! @p 123 !+ @p ! . 456 dup or a! . @+ 2* @+ . 2/ + ! ."
            2 @=? a ?res
            123 @=? memory ?res ! 0
            456 @=? memory ?res ! 1
            474 @=? memory ?res ! 2
            7 @=? p ?res
            unchanged [b, r, s, t]
case_ret = do let ?res = run "call 2 . . . . ; . . ."
              1 @=? p ?res
              unchanged [a, b, r, s, t]
case_jump = do let ?res = run "jump 42"
               42 @=? p ?res
               unchanged [a, b, r, s, t]
case_call = do let ?res = run "call 10"
               1 @=? r ?res
               10 @=? p ?res
               unchanged [a, b, s, t]
case_unext = do let ?res = run ". . unext ."
                1 @=? p ?res
                unchanged [a, b, r, s, t]
case_unext' = do let ?res = run "@p push . . 41 @+ . . unext"
                 3 @=? p ?res
                 42 @=? a ?res
                 unchanged [b, r, s, t]
case_if = do let ?res = run "if 42"
             1 @=? p ?res
             unchanged [a, b, r, s, t]
case_if' = do let ?res = run "@p if 42 10"
              42 @=? p ?res
              unchanged [a, b, r, s]
case_minusIf = do let ?res = run "-if 42"
                  42 @=? p ?res
                  unchanged [a, b, r, s, t]
case_minusIf' = do let ?res = run "- -if 42"
                   1 @=? p ?res
                   unchanged [a, b, r, s]
case_fetchP = do let ?res = run "@p . . . 42"
                 2 @=? p ?res
                 42 @=? t ?res
                 unchanged [a, b, r, s]
case_fetchPlus = do let ?res = run "@+ . . ."
                    1 @=? a ?res
                    memory ?res ! 0 @=? t ?res
                    unchanged [b, r, s]
case_fetchB = do let ?res = run "@b . . ."
                 memory ?res ! 0 @=? t ?res
                 unchanged [b, r, s]
case_fetch = do let ?res = run "@ . . ."
                memory ?res ! 0 @=? t ?res
                unchanged [b, r, s]
case_storeP = do let ?res = run "@p !p . . 42"
                 3 @=? p ?res
                 42 @=? memory ?res ! (p ?res - 1)
                 unchanged [a, b, r, s]
case_storePlus = do let ?res = run "@p !+ . . 42"
                    1 @=? a ?res
                    2 @=? p ?res
                    42 @=? memory ?res ! 0
                    unchanged [b, r, s]
case_storePlus' = do let ?res = run "@p @p a! . 42 10 !+ . . ."
                     11 @=? a ?res
                     4 @=? p ?res
                     42 @=? memory ?res ! 10
                     unchanged [b, r, s]
case_storePlus'' = do let ?res = run "dup or a! @p 123 !+ @p ! . 456"
                      1 @=? a ?res
                      123 @=? memory ?res ! 0
                      456 @=? memory ?res ! 1
                      unchanged [b, r, s, t]
case_storeB = do let ?res = run "@p !b . . 42"
                 2 @=? p ?res
                 42 @=? memory ?res ! 0
                 unchanged [a, b, r, s]
case_storeB' = do let ?res = run "@p @p b! . 42 10 !b . . ."
                  4 @=? p ?res
                  42 @=? memory ?res ! 10
                  10 @=? b ?res
                  unchanged [a, r, s]
case_store = do let ?res = run "@p ! . . 42"
                2 @=? p ?res
                42 @=? memory ?res ! 0
                unchanged [a, b, r, s]
case_store' = do let ?res = run "@p @p a! . 42 10 ! . . ."
                 10 @=? a ?res
                 4 @=? p ?res
                 42 @=? memory ?res ! 10
                 unchanged [b, r, s]
case_store'' = do let ?res = run "dup or a! @p 123 ! . . ."
                  0 @=? a ?res
                  123 @=? memory ?res ! 0
                  unchanged [b, r, s, t]
case_times2 = do let ?res = run "@p 2* . . 2"
                 4 @=? t ?res
                 2 @=? p ?res
                 unchanged [a, b, r, s]
case_div2 = do let ?res = run "@p 2/ . . 4"
               2 @=? t ?res
               2 @=? p ?res
               unchanged [a, b, r, s]
case_not = do let ?res = run "- . . ."
              (- 1) @=? t ?res
              1 @=? p ?res
              unchanged [a, b, r, s]
case_not' = do let ?res = run "@p - . . 42"
               complement 42 @=? t ?res
               2 @=? p ?res
               unchanged [a, b, r, s]
case_plus = do let ?res = run "@p @p . + 12 30"
               42 @=? t ?res
               3 @=? p ?res
               unchanged [a, b, r, s]
case_and = do let ?res = run "@p @p and . 12 30"
              12 .&. 30 @=? t ?res
              3 @=? p ?res
              unchanged [a, b, r, s]
case_or = do let ?res = run "@p @p or . 12 30"
             12 `xor` 30 @=? t ?res
             3 @=? p ?res
             unchanged [a, b, r, s]
case_drop = do let ?res = run "@p @p drop . 1 2"
               1 @=? t ?res
               3 @=? p ?res
               unchanged [a, b, r, s]
case_dup = do let ?res = run "@p dup . . 42"
              42 @=? t ?res
              42 @=? s ?res
              2 @=? p ?res
              unchanged [a, b, r]
case_dup' = do let ?res = run "@p dup or . 42"
               0 @=? t ?res
               2 @=? p ?res
               unchanged [a, b, r, s]
case_pop = do let ?res = run "call 2 0 pop . . ."
              1 @=? t ?res
              unchanged [a, b, r, s]
case_over = do let ?res = run "@p @p over . 1 2"
               1 @=? t ?res
               2 @=? s ?res
               3 @=? p ?res
               unchanged [a, b, r]
case_a = do let ?res = run "@p a! a . 42"
            42 @=? a ?res
            42 @=? t ?res
            2 @=? p ?res
            unchanged [b, r, s]
case_nop = do let ?res = step $ run ". . . ."
              1 @=? p ?res
              unchanged [a, b, r, s, t]
case_push = do let ?res = run "@p push . . 42"
               42 @=? r ?res
               2 @=? p ?res
               unchanged [a, b, s, t]
case_setB = do let ?res = run "@p b! . . 42"
               42 @=? b ?res
               2 @=? p ?res
               unchanged [a, r, s, t]
case_setA = do let ?res = run "@p a! . . 42"
               42 @=? a ?res
               2 @=? p ?res
               unchanged [b, r, s, t]
