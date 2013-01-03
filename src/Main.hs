module Main where

import           Control.Monad.Random            (evalRandIO)

import           Data.List                       (find)

import           Language.Forth.Distance         (Distance, registers)
import           Language.Forth.Interpreter      (eval)
import           Language.Forth.Parse            ()
import           Language.Forth.State            (State (..), startState)
import           Language.Forth.Synthesis        (Program, defaultMutations,
                                                  defaultOps, evaluate, load)

import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.Synthesis    (Problem (..), runningBest,
                                                  synthesizeMhList)

main :: IO ()
main = verbose

good :: (Program, Double) -> Bool
good (_, val) = val >= 0.5

verbose :: IO ()
verbose = do ls <- evalRandIO (synthesizeMhList inclusiveOr)
             mapM_ print . zip ls . takeWhile (not . good) $ runningBest ls

run :: IO ()
run = evalRandIO (synthesizeMhList inclusiveOr) >>= print . find good . runningBest

test :: Distance -> String -> String -> State -> Double
test distance p₁ p₂ input = let r₁ = eval $ load (read p₁) input
                                r₂ = eval $ load (read p₂) input in
                            distance r₁ r₂ 
                                     
inclusiveOr :: Problem Program
inclusiveOr = Problem { score = evaluate program cases distance
                      , prior = Distr.replicate 8 defaultOps
                      , jump  = defaultMutations }
  where program = read "over over or a! and a or"
        cases = [startState {t = 0, s = 123}, startState {t = maxBound, s = 123},
                 startState {t = 1, s = 123}, startState {t = maxBound - 1, s = 123}]
        distance = registers [t]
