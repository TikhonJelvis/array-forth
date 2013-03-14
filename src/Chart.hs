{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow                   ((&&&))
import           Control.Monad.Random            (evalRandIO)

import           Data.Bits                       (complement)
import           Data.Functor                    ((<$>))
import           Data.Function                   (on)
import           Data.List
import           Data.Monoid                     ((<>), Sum (..), Monoid)

import           Graphics.Rendering.Chart.Simple

import           Language.ArrayForth.Distance    (Distance, matching, registers)
import           Language.ArrayForth.Interpreter (eval)
import           Language.ArrayForth.Parse       ()
import           Language.ArrayForth.Program     (Program, load, readProgram)
import qualified Language.ArrayForth.Stack       as S
import           Language.ArrayForth.State       (State (..), startState)
import           Language.ArrayForth.Synthesis   (DefaultScore (..),
                                                  defaultMutations, defaultOps,
                                                  evaluate, trace, withPerformance)
import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.Synthesis    (Problem (..), Score (..),
                                                  runningBest, synthesizeMhList)
import           Text.Printf

range :: [Double]
range = [0..]

main :: IO ()
main = run

good :: Score s => (Program, s) -> Bool
good (_, val) = toScore val >= 0

run :: IO ()
run = do programs <- evalRandIO $ synthesizeMhList traceOr
         let process = sample 25 . movingAvg 50 . drop 10
             results = snd . head <$> group programs
             scores = take 2500 . process $ map toScore results
             correctness = take (length scores) . process $ map corr results
         printf "Number of points:\n score: %d\n correctness: %d.\n" (length scores) (length correctness)
         printf "Result: %s.\n" . show $ programs !! (25 * 2500)
         plotPDF "out.pdf" range scores Solid correctness Solid

corr :: DefaultScore -> Double
corr (DefaultScore a _) = a

sample :: Int -> [a] -> [a]
sample _ [] = []
sample n (x:xs) = x : sample n (drop n xs)

movingAvg :: Fractional a => Int -> [a] -> [a]
movingAvg _ []             = [0]
movingAvg window ls@(_:xs) = (sum start / genericLength start) : movingAvg window xs
  where start = take window ls

cases :: [State]
cases = [startState {t = 0, s = 123}, startState {t = maxBound, s = 123},
         startState {t = 1, s = 123}, startState {t = maxBound - 1, s = 123}]

orSpec :: Program
orSpec = "over over or a! and a or"

inclusiveOr :: Problem Program DefaultScore
inclusiveOr = Problem { score = evaluate orSpec cases distance
                      , prior = Distr.constant orSpec
                      , jump  = defaultMutations }
  where complemented σ₁ σ₂@State {t = t₂} =
          Sum . negate . getSum . registers [t] σ₁ $ σ₂ {t = complement t₂}
        distance = registers [t] <> complemented

traceOr :: Problem Program DefaultScore
traceOr = Problem { score = trace orSpec cases $ withPerformance sc
                  , prior = Distr.constant orSpec
                  , jump = defaultMutations }
  where sc = matching (s &&& t) <> (registers [t] `on` last)