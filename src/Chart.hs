{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
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

import           Options.Applicative

import           Text.Printf

data Options = Options { out                :: Maybe FilePath
                       , problem            :: Problem Program DefaultScore
                       , points, resolution :: Int
                       , maxScore           :: Maybe Double }

options :: Parser Options
options = Options
          <$> nullOption (long "out"
                       <> short 'o'
                       <> value Nothing
                       <> metavar "PATH"
                       <> reader (return . Just)
                       <> help "Filepath for the resulting chart.")
          <*> nullOption (long "problem"
                       <> short 'p'
                       <> value inclusiveOr
                       <> metavar "NAME"
                       <> eitherReader parseProblem
                       <> help problemHelp)
          <*> option     (long "samples"
                       <> short 's'
                       <> value 2500
                       <> metavar "SAMPLES"
                       <> help "The number of samples to take. Each sample corresponds to something like ~6k programs considered.")
          <*> option     (long "resolution"
                       <> short 'r'
                       <> value 25
                       <> metavar "N"
                       <> help "Sample every N generated candidate programs.")
          <*> nullOption (long "max"
                       <> short 'x'
                       <> value Nothing
                       <> metavar "MAX_SCORE"
                       <> reader (return . Just . read)
                       <> help "Stop at the given score.")

-- I wish existential types were better :/
problems :: [(String, Problem Program DefaultScore)]
problems = [("traceOr", traceOr), ("inclusiveOr", inclusiveOr)]

problemHelp :: String
problemHelp = printf "The problem to run. Currently, the valid choices are:\n%s" names
  where names = init . unlines $ map (((replicate 30 ' ' ++ "- ") ++ ) . fst) problems

parseProblem :: String -> Either String (Problem Program DefaultScore)
parseProblem problem = case lookup problem problems of
  Just p  -> return p
  Nothing -> Left $ printf "Problem name %s is not recognized." problem

range :: [Double]
range = [0..]

main :: IO ()
main = execParser go >>= run
  where go = info (helper <*> options)
             (fullDesc
           <> progDesc "Synthesize arrayForth programs using different strategies and graph the performances of the evaluation function."
           <> header "chart - chart the performance of MCMC synthesis")

good :: Score s => (Program, s) -> Bool
good (_, val) = toScore val >= 0

run :: Options -> IO ()
run Options {..} =
  do programs <- evalRandIO $ synthesizeMhList problem
     let getMax      = maybe id (takeWhile . (<)) maxScore
         process     = take points . sample resolution . movingAvg (2 * resolution) . drop 10
         results     = snd . head <$> group programs
         scores      = process . getMax $ map toScore results
         correctness = take (length scores) . process $ map corr results
     printf "Result: %s.\n" . show $ programs !! (resolution * points)
     case out of
       Just filepath -> plotPDF filepath range scores Solid correctness Solid
       Nothing       -> return ()

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
         startState {t = 1, s = 123}, startState {t = maxBound - 1, s = 123},
         startState {t = 37, s = 123}, startState {t = 52, s = 123}]
        
orSpec :: Program
orSpec = "over over or a! and a or"

inclusiveOr :: Problem Program DefaultScore
inclusiveOr = Problem { score = evaluate orSpec cases distance
                      , prior = Distr.constant orSpec
                      , jump  = defaultMutations }
  where complemented σ₁ σ₂@State {t = t₂} =
          Sum . negate . getSum . registers [t] σ₁ $ σ₂ {t = complement t₂}
        distance = registers [t] 

traceOr :: Problem Program DefaultScore
traceOr = Problem { score = trace orSpec cases $ withPerformance sc
                  , prior = Distr.constant orSpec
                  , jump = defaultMutations }
  where sc = matching (s &&& t) <> (registers [t] `on` last)
