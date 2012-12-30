{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Forth.Synthesis where

import           Control.Arrow                   (first)
import           Control.Monad.Random            (Random, random, randomR)

import           Data.Functor                    ((<$>))
import           Data.List                       (find, genericLength, (\\))
import           Data.List.Split                 (chunk)

import           Language.Forth.Distance
import           Language.Forth.Instructions
import           Language.Forth.Interpreter
import           Language.Forth.State

import           Language.Synthesis.Distribution (Distr (..), mix,
                                                  negativeInfinity, randInt,
                                                  uniform)
import           Language.Synthesis.Mutations    hiding (mix)
import qualified Language.Synthesis.Mutations    as M

-- | Represents a single instruction as viewed by the
-- synthesizer. This can be an opcode, a numeric literal or a token
-- representing an unused slot.
data Instruction = Opcode Opcode
                 | Number F18Word
                 | Unused deriving Eq

-- | A program to be manipulated by the MCMC synthesizer
type Program = [Instruction]

instance Show Instruction where
  show (Opcode op) = show op
  show (Number n)  = show n
  show Unused      = "_"
  showList = (++) . unwords . map show

-- | Takes a program as handled by the synthesizer and makes it native
-- by turning literal numbers into @p and fixing any issues with
-- instructions going into the last slot as well as prepending
-- nops before + instructions.
toNative :: Program -> NativeProgram
toNative = concatMap toInstrs . chunk 4 . addNops . concatMap nopsPlus . filter (/= Unused)
  where nop = Opcode Nop
        addNops program
          | length program < 4 = program
          | slot3 op4          = take 4 program ++ addNops (drop 4 program)
          | otherwise          = take 3 program ++ [nop] ++ addNops (drop 3 program)
          where op4 = case program !! 3 of
                  Opcode op -> op
                  Number{}  -> FetchP
                  Unused    -> error "Cannot have unused slot in fixSlot3!"
        nopsPlus (Opcode Plus) = [nop, Opcode Plus]
        nopsPlus x             = [x]
        toInstrs = convert . addFetchP
        addFetchP [] = ([], [])
        addFetchP (n@Number{} : rest) =
          let (instrs, consts) = addFetchP rest in (Opcode FetchP : instrs, n : consts)
        addFetchP (instr : rest) =
          let (instrs, consts) = addFetchP rest in (instr : instrs, consts)
        convert ([Opcode a, Opcode b, Opcode c, Opcode d], numbers) =
          Instrs a b c d : map (\ (Number n) -> Constant n) numbers
        convert (instrs, consts) = convert (take 4 $ instrs ++ repeat nop, consts)

-- | Gets a synthesizer program from a native program. Currently does
-- not support jumps.
fromNative :: NativeProgram -> Program
fromNative = fixNumbers . concatMap extract
  where extract (Instrs a b c d) = [Opcode a, Opcode b, Opcode c, Opcode d]
        extract (Constant n)     = [Number n]
        extract _                = error "Jumps are not yet supported!"
        fixNumbers [] = []
        fixNumbers (Opcode FetchP : rest) = case find isNumber rest of
          Just n  -> n : (fixNumbers $ rest \\ [n])
          Nothing -> Opcode FetchP : fixNumbers rest
        fixNumbers (x : rest)   = x : fixNumbers rest
        isNumber Number{} = True
        isNumber _        = False

-- | Runs a given program from the default starting state.
runProgram :: State -> Program -> State
runProgram start = runNativeProgram start . toNative

-- | Loads the given synthesizer-friendly program into the given
-- state.
load :: Program -> State -> State
load prog state = setProgram 0 (toNative prog) state

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
          Just res -> (-score output (last res), time - countTime res)
          Nothing  -> (read "-Infinity", read "-Infinity") -- TODO: Do this more elegantly?

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
