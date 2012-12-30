module Main where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Monad.Random            (evalRandIO)

import           Data.List                       (find)

import           Language.Forth.Distance         (Distance, registers)
import           Language.Forth.Interpreter      (throttle)
import           Language.Forth.Parse            ()
import           Language.Forth.State            (State (..), startState)
import           Language.Forth.Synthesis        (Program, defaultMutations,
                                                  defaultOps, evaluate, load)

import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.Synthesis    (Problem (..), runningBest,
                                                  synthesizeMhList)

main :: IO ()
main = evalRandIO (synthesizeMhList inclusiveOr) >>= print . find good . runningBest
  where good (_, score) = score >= 0

test :: Int -> Distance -> String -> String -> State -> Double
test steps distance p₁ p₂ input = let r₁ = throttle steps (load (read p₁) startState)
                                      r₂ = throttle steps (load (read p₂) startState) in
                                     case distance <$> r₁ <*> r₂ of
                                       Just d  -> d
                                       Nothing -> read "Infinity"

inclusiveOr :: Problem Program
inclusiveOr = Problem { score = evaluate program cases distance
                      , prior = Distr.replicate 8 defaultOps
                      , jump  = defaultMutations }
  where program = read "over over or a! and a or"
        cases = [startState {t = 0, s = 123}, startState {t = maxBound, s = 123},
                 startState {t = 1, s = 123}, startState {t = maxBound - 1, s = 123}]
        distance = registers [t]
