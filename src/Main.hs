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
        cases = [startState {t = 0 }, startState {t = maxBound},
                 startState {t = 1}, startState {t = maxBound - 1}]
        distance = registers [t]
