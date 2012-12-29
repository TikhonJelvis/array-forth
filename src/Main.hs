module Main where

import           Control.Monad.Random            (evalRandIO)

import           Language.Forth.Distance         (registers)
import           Language.Forth.Parse            ()
import           Language.Forth.State            (State (..), startState)
import           Language.Forth.Synthesis        (Program, defaultMutations,
                                                  defaultOps, evaluate)

import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.Synthesis    (Problem (..), runningBest,
                                                  synthesizeMhList)

main :: IO ()
main = evalRandIO (synthesizeMhList inclusiveOr) >>= print . (!! 10000) . runningBest

inclusiveOr :: Problem Program
inclusiveOr = Problem { score = evaluate program cases distance
                      , prior = Distr.replicate 8 defaultOps
                      , jump  = defaultMutations}
  where program = read "over over or a! and a or"
        cases = [startState {t = 0, s = 123}, startState {t = maxBound, s = 123},
                 startState {t = 1, s = 123}, startState {t = maxBound - 1, s = 123}]
        distance = registers [t]
