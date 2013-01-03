{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.ArrayForth.Synthesis where

import           Control.Arrow                   (first)
import           Control.Monad.Random            (Random, random, randomR)

import           Data.Functor                    ((<$>))
import           Data.List                       (genericLength, (\\))

import           Language.ArrayForth.Distance
import           Language.ArrayForth.Interpreter
import           Language.ArrayForth.Opcode
import           Language.ArrayForth.Program
import           Language.ArrayForth.State

import           Language.Synthesis.Distribution (Distr (..), mix,
                                                  negativeInfinity, randInt,
                                                  uniform)
import           Language.Synthesis.Mutations    hiding (mix)
import qualified Language.Synthesis.Mutations    as M

-- | Given a specification program and some inputs, evaluate a program
-- against the specification for both performance and correctness.
evaluate :: Program -> [State] -> Distance -> Program -> Double
evaluate spec inputs score program =
  0.1 * (10 * sum correctness + sum performance / genericLength inputs)
  where specs = stepProgram . load spec <$> inputs
        progs = stepProgram . load program <$> inputs
        cases = zip3 (last <$> specs) (length <$> specs) (countTime <$> specs)
        (correctness, performance) = unzip $ zipWith test progs cases
        test prog (output, steps, time) = case throttle steps prog of
          Right res -> calc res
          Left res  -> let (a, b) = calc res in (a - 1e10, b - 1e10)
          where calc res = (-score output (last res), time - countTime res)

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
                  (uniform instrs, genericLength instrs)]
  where instrs = map Opcode $ filter (not . isJump) opcodes \\ [Unext, Exec, Ret]
        constants = let Distr {sample, logProbability} = randInt (0, maxBound)
                        logProb (Number n) = logProbability n
                        logProb _          = negativeInfinity in
                    Distr { sample = Number <$> sample
                          , logProbability = logProb }

-- | The default mutations to try. For now, this will either change an
-- instruction or swap two instructions in the program, with equal
-- probability.
defaultMutations :: Mutation Program
defaultMutations = M.mix [(mutateInstruction defaultOps, 1), (swapInstructions, 1)]
