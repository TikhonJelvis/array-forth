{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.ArrayForth.Synthesis where

import           Control.Arrow                   (first)
import           Control.Monad.Random            (Random, random, randomR)

import           Data.Function                   (on)
import           Data.Functor                    ((<$>))
import           Data.List                       (elemIndices, genericLength, (\\))
import           Data.Monoid                     (Monoid (..), (<>))

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
import           Language.Synthesis.Synthesis    (Score (..))

import           Text.Printf

-- | A score type that contains a correctness value and a performance
-- value.
data DefaultScore = DefaultScore Double Double deriving (Ord, Eq)

instance Score DefaultScore where
  toScore (DefaultScore correctness performance) = correctness + 0.1 * performance

instance Show DefaultScore where show (DefaultScore a b) = printf "<%.2f, %.2f>" a b

instance Monoid DefaultScore where
  mempty = DefaultScore 0 0
  DefaultScore c₁ p₁ `mappend` DefaultScore c₂ p₂ = DefaultScore (c₁ + c₂) (p₁ + p₂)

-- | Creates an evaluation function from a spec, a set of inputs and a
-- function for comparing program traces.
trace :: Monoid score => Program -> [State] -> (Trace -> Trace -> score) -> Program -> score
trace spec inputs score program = mconcat $ zipWith score specs throttled
  where specs   = stepProgram . load spec <$> inputs
        results = stepProgram . load program <$> inputs
        throttled = zipWith go specs results
          where go spec trace = either id id $ throttle (length spec) trace

-- | Using a given correctness measure, produce a score also
-- containing performance.
withPerformance :: Score s => (Trace -> Trace -> s) -> (Trace -> Trace -> DefaultScore)
withPerformance score spec result = DefaultScore (toScore $ score spec res) performance
  where res = either id id $ throttle (length spec) result
        performance = case throttle (length spec) result of
          Right res -> (countTime spec - countTime res) / 10
          Left  res -> countTime spec - countTime res - 1e10

-- | Given a specification program and some inputs, evaluate a program
-- against the specification for both performance and
-- correctness. Normalize the score based on the number of test cases.
evaluate :: Program -> [State] -> (State -> State -> Distance) -> Program -> DefaultScore
evaluate spec inputs distance =
  normalize . trace spec inputs (withPerformance (distance `on` last))
  where normalize (DefaultScore c p) = DefaultScore (c / len) (p / len)
        len = genericLength inputs

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
  where instrs = map Opcode $ filter (not . isJump) opcodes \\ [Unext, Nop]
        constants = let Distr {..} = randInt (0, maxBound)
                        logProb (Number n) = logProbability n
                        logProb _          = negativeInfinity in
                    Distr { sample = Number <$> sample
                          , logProbability = logProb }

pairs :: [(Instruction, Instruction)]
pairs = map (\ (a, b) -> (Opcode a, Opcode b))
        [ (SetA, ReadA)
        , (Push, Pop)
        , (Over, Drop) ]

removePairs :: Distr Instruction -> Mutation Program
removePairs instrDistr program =
  mix [(mutateInstructionsAt instrDistr is program, 1.0) | is <- findPairs program]
  where findPairs program = do (a, b) <- pairs
                               indexA <- elemIndices a program
                               indexB <- elemIndices b program
                               return [indexA, indexB]

-- | The default mutations to try. For now, this will either change an
-- instruction or swap two instructions in the program, with equal
-- probability.
defaultMutations :: Mutation Program
defaultMutations = M.mix [(mutateInstruction defaultOps, 1), (swapInstructions, 1)]
