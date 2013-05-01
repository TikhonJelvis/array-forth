{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow                   ((&&&), second)
import           Control.Monad.Random            (evalRandIO)

import           Data.Bits                       (complement)
import           Data.Function                   (on)
import           Data.List                       (find)
import           Data.Monoid                     (Sum (..))

import           Options.Applicative

import           Language.ArrayForth.Distance    (Distance, matching, registers)
import           Language.ArrayForth.Interpreter (eval)
import           Language.ArrayForth.Parse       ()
import           Language.ArrayForth.Program     (Program, load, readProgram)
import qualified Language.ArrayForth.Stack       as S
import           Language.ArrayForth.State       (State (..), startState)
import           Language.ArrayForth.Synthesis   (DefaultScore (..), defaultMutations, defaultOps,
                                                  evaluate, trace, withPerformance)

import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.Synthesis    (Problem (..), Score (..), runningBest,
                                                  synthesizeMhList)

data Options = Options { verbose :: Bool }

options :: Parser Options
options =  Options <$> switch (long "verbose" <>
                               short 'v' <>
                               help "Print intermediate state to STDOUT.")

specP :: Parser Program
specP = argument (either (const Nothing) Just . readProgram) (metavar "SPEC")

main :: IO ()
main = do Options { verbose } <- execParser go
          if verbose then verbosely else run
  where go = info (helper <*> options)
             (fullDesc <>
              progDesc "Synthesize arrayForth programs using MCMC." <>
              header "mcmc-demo - simple synthesis with MCMC")

good :: Score s => (Program, s) -> Bool
good (_, val) = toScore val >= 0.5

verbosely :: IO ()
verbosely = do ls <- evalRandIO (synthesizeMhList inclusiveOr)
               mapM_ (print . second toScore . fst) . zip ls . takeWhile (not . good) $ runningBest ls

run :: IO ()
run = evalRandIO (synthesizeMhList inclusiveOr) >>= print . find good . runningBest

test :: (State -> State -> t) -> String -> String -> State -> t
test distance p₁ p₂ input = let r₁ = eval $ load (read p₁) input
                                r₂ = eval $ load (read p₂) input in
                            distance r₁ r₂

orSpec :: Program
orSpec = "over over or a! and a or"

cases :: [State]
cases = [startState {t = 0, s = 123}, startState {t = maxBound, s = 123},
         startState {t = 1, s = 123}, startState {t = maxBound - 1, s = 123},
         startState {t = 37, s = 123}, startState {t = 52, s = 123}]

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

-- bitwiseSwap :: Problem Program DefaultScore
-- bitwiseSwap = Problem { score = evaluate program cases distance
--                       , prior = Distr.constant program
--                       , jump = defaultMutations }
--   where program = "a! over over . a - and . push a and . pop over over . or push and . pop or . ."
--         cases = [ startState {t = 46, s = 18, dataStack = st 43}
--                 , startState {t = 232, s = 123, dataStack = st 0}
--                 , startState {t = 2352, s = 123, dataStack = st 1}
--                 , startState {t = maxBound - 5, s = 123, dataStack = st 13}
--                 ]
--         distance = registers [t]
--         st = S.push S.empty
