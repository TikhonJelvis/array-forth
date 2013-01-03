{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.ArrayForth.Program where

import           Data.Functor                      ((<$>))
import           Data.List                         (find, (\\))
import           Data.List.Split                   (chunk)
import           Data.String                       (IsString, fromString)

import           Language.ArrayForth.Interpreter
import           Language.ArrayForth.NativeProgram
import           Language.ArrayForth.Opcode
import qualified Language.ArrayForth.Parse         as P
import           Language.ArrayForth.State         (State, setProgram)

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

-- | Tries to parse the given string as an instruction, which can
-- either be a number, an opcode or "_" representing Unused.
readInstruction :: String -> Either P.ParseError Instruction
readInstruction "_"                  = Right Unused
readInstruction str | P.isNumber str = Number <$> P.readWord str
                    | otherwise      = Opcode <$> readOpcode str

-- | Reads a program in the synthesizer's format.
readProgram :: String -> Either P.ParseError Program
readProgram = mapM readInstruction . words

instance Read Program where
  readsPrec _ str = [(result, "")]
    where result = case readProgram str of
            Right res -> res
            Left  err -> error $ show err

instance IsString Program where fromString = read

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
