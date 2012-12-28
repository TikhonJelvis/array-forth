{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Forth.Synthesis where

import           Control.Arrow                   (first)
import           Control.Monad.Random            (Random, random, randomR)

import           Data.Functor                    ((<$>))
import           Data.List                       (find, genericLength, (\\))
import           Data.List.Split                 (chunk)

import           Language.Forth.Distance
import           Language.Forth.Instructions
import           Language.Forth.Interpreter
import           Language.Forth.Parse
import           Language.Forth.State

import           Language.Synthesis.Distribution (Distr (..), mix,
                                                  negativeInfinity, randInt,
                                                  uniform)

-- | Represents a single instruction as viewed by the
-- synthesizer. This can be an opcode, a numeric literal or a token
-- representing an unused slot.
data Instruction = Opcode Opcode
                 | Number F18Word
                 | Unused deriving (Show, Eq)

-- | Does the given instruction corresponds to a constant number?
isNumber :: Instruction -> Bool
isNumber Number{} = True
isNumber _        = False

-- | A program to be manipulated by the MCMC synthesizer
type Program = [Instruction]

instance Read Program where readsPrec _ str = [(fromNative $ read str, "")]

-- | Takes a program as handled by the synthesizer and makes it native
-- by turning literal numbers into @p and fixing any issues with
-- instructions going into the last slot.
toNative :: Program -> NativeProgram
toNative = concatMap (toInstrs . addFetchP) . chunk 4 . fixSlot3 . filter (/= Unused)
  where addFetchP [] = ([], [])
        addFetchP (n@Number{} : rest) =
          let (instrs, consts) = addFetchP rest in (Opcode FetchP : instrs, n : consts)
        addFetchP (instr : rest) =
          let (instrs, consts) = addFetchP rest in (instr : instrs, consts)
        toInstrs ([Opcode a, Opcode b, Opcode c, Opcode d], numbers) =
          Instrs a b c d : map (\ (Number n) -> Constant n) numbers
        toInstrs (instrs, consts) =
          toInstrs (take 4 $ instrs ++ repeat (Opcode Nop), consts)

-- | Gets a synthesizer program from a native program. Currently does
-- not support jumps.
fromNative :: NativeProgram -> Program
fromNative = fixNumbers . concatMap extract
  where extract (Instrs a b c d) = [Opcode a, Opcode b, Opcode c, Opcode d]
        extract (Constant n)     = [Number n]
        extract _                = error "Jumps are not yet supported!"
        fixNumbers [] = []
        fixNumbers (Opcode FetchP : rest) = case find isNumber rest of
          Just n  -> n : (fixNumbers $ rest \\ [n])
          Nothing -> Opcode FetchP : fixNumbers rest
        fixNumbers (x : rest)   = x : fixNumbers rest

-- | Take a program and ensure that only instructions allowed in the
-- last slot go there by adding nops as necessary.
fixSlot3 :: Program -> Program
fixSlot3 program
  | length program < 4 = program
  | slot3 op4          = take 4 program ++ fixSlot3 (drop 4 program)
  | otherwise          = take 3 program ++ [Opcode Nop] ++ fixSlot3 (drop 3 program)
  where op4 = case program !! 3 of
          Opcode op -> op
          Number{}  -> FetchP
          Unused    -> error "Cannot have unused slot in fixSlot3!"

-- | Returns a measure of the quality of the program. For now this is
-- only based on the performance of the program.
runtime :: Program -> Double
runtime = runningTime . toNative

-- | Runs a given program from the default starting state.
runProgram :: State -> Program -> State
runProgram start = runNativeProgram start . toNative

-- | Compare a program against an input/output pair using the given
-- distance function.
test :: Distance -> Program -> (State, State) -> Double
test distance program (input, output) = distance output $ runProgram input program

-- | Given a specification program and some inputs, evaluate a program
-- against the specification for both performance and correctness.
evaluate :: Program -> [State] -> Distance -> Program -> Double
evaluate spec inputs score program = 10 * correctness + performance
  where pairs = zip inputs $ map (`runProgram` spec) inputs
        correctness = -sum (test score program <$> pairs)
        performance = runtime spec - runtime program

-- I need this so that I can get a distribution over Forth words.
instance Random F18Word where
  randomR (start, end) gen =
    first fromInteger $ randomR (fromIntegral start, fromIntegral end) gen
  random = randomR (0, maxBound)

-- | The default distribution of instructions. For now, we do not
-- support any sort of jumps. All the other possible instructions
-- along with constant numbers and unused slots are equally
-- likely. The numeric value of constants is currently a uniform
-- distribution over 18-bit words.
defaultOps :: Distr Instruction
defaultOps = mix [(constants, 1.0), (uniform [Unused], 1.0),
                  (uniform  instrs, genericLength instrs)]
  where instrs = map Opcode $ filter (not . isJump) opcodes
        constants = let Distr {sample, logProbability} = randInt (0, maxBound)
                        logProb (Number n) = logProbability n
                        logProb _          = negativeInfinity in
                    Distr { sample = Number <$> sample
                          , logProbability = logProb }
