{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.ArrayForth.Program where

import           Control.Monad                     ((<=<))

import           Data.Functor                      ((<$>))
import           Data.List                         (find, (\\))
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
                 | Jump Opcode Addr
                 | Number F18Word
                 | Unused deriving Eq

-- | A program to be manipulated by the MCMC synthesizer
type Program = [Instruction]

instance Show Instruction where
  show (Opcode op)    = show op
  show (Jump op addr) = show op ++ " " ++ show addr
  show (Number n)     = show n
  show Unused         = "_"
  showList = (++) . unwords . map show

-- | Tries to parse the given string as an instruction, which can
-- either be a number, an opcode or "_" representing Unused.
readInstruction :: String -> Either P.ParseError Instruction
readInstruction "_"                  = Right Unused
readInstruction str | P.isNumber str = Number <$> P.readWord str
                    | otherwise      = Opcode <$> readOpcode str

-- | Reads a program in the synthesizer's format.
readProgram :: String -> Either P.ParseError Program
readProgram = fixJumps <=< mapM readInstruction . words
  where fixJumps [] = Right []
        fixJumps (Opcode op : rest) | isJump op = case rest of
          Number n : program -> (Jump op n :) <$> fixJumps program
          _                  -> Left . P.NoAddr $ show op
        fixJumps (good : rest) = (good :) <$> fixJumps rest

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
toNative = (>>= toInstrs) . splitWords bound . fixSlot3 . (>>= nopsPlus) . jumps . filter (/= Unused)
  where nop = Opcode Nop
        jumps []                                        = []
        jumps (Opcode op : Number n : rest) | isJump op = Jump op n : jumps rest
        jumps (op : rest)                               = op : jumps rest
        bound Jump{} = True
        bound _      = False
        nopsPlus (Opcode Plus) = [nop, Opcode Plus]
        nopsPlus x             = [x]
        fixSlot3 program
          | length program < 4 = program
          | validOp4           = take 4 program ++ fixSlot3 (drop 4 program)
          | otherwise          = take 3 program ++ [nop] ++ fixSlot3 (drop 3 program)
          where validOp4 = case program !! 3 of Opcode op -> slot3 op
                                                Number{}  -> True
                                                _         -> False
        toInstrs ls = let (ops, numbers) = addFetchP ls in
          convert ops : map (\ (Number n) -> Constant n) numbers
        addFetchP [] = ([], [])
        addFetchP (n@Number{} : rest) =
          let (instrs, consts) = addFetchP rest in (Opcode FetchP : instrs, n : consts)
        addFetchP (instr : rest) =
          let (instrs, consts) = addFetchP rest in (instr : instrs, consts)
        convert [Opcode a, Opcode b, Opcode c, Opcode d] = Instrs a b c d
        convert [Opcode a, Opcode b, Jump c addr]        = Jump3 a b c addr
        convert [Opcode a, Jump b addr]                  = Jump2 a b addr
        convert [Jump a addr]                            = Jump1 a addr
        convert instrs                                   = convert . take 4 $ instrs ++ repeat nop

-- | Gets a synthesizer program from a native program. Currently does
-- not support jumps.
fromNative :: NativeProgram -> Program
fromNative = fixNumbers . concatMap extract
  where extract (Instrs a b c d)   = [Opcode a, Opcode b, Opcode c, Opcode d]
        extract (Jump3 a b c addr) = [Opcode a, Opcode b, Jump c addr]
        extract (Jump2 a b addr)   = [Opcode a, Jump b addr]
        extract (Jump1 a addr)     = [Jump a addr]
        extract (Constant n)       = [Number n]
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
