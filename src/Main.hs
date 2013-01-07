{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Random            (evalRandIO)

import           Data.List                       (find)

import           Options.Applicative

import           Language.ArrayForth.Distance    (Distance, registers)
import           Language.ArrayForth.Interpreter (eval)
import           Language.ArrayForth.Parse       ()
import           Language.ArrayForth.Program     (Program, load, readProgram)
import           Language.ArrayForth.State       (State (..), startState)
import           Language.ArrayForth.Synthesis   (defaultMutations, defaultOps,
                                                  evaluate)

import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.Synthesis    (Problem (..), runningBest,
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
  where go = info (helper <*> options) (fullDesc <>
                                        progDesc "Synthesize arrayForth programs using MCMC." <>
                                        header "mcmc-demo - simple synthesis with MCMC")

good :: (Program, Double) -> Bool
good (_, val) = val >= 0.5

verbosely :: IO ()
verbosely = do ls <- evalRandIO (synthesizeMhList inclusiveOr)
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
