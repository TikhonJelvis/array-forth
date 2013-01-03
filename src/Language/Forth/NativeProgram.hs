{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Forth.NativeProgram where

import           Data.Bits             (shift, (.&.), (.|.))

import           Language.Forth.Opcode

-- | Represents a word in memory. This word can either contain
-- opcodes, opcodes and a jump address or just a constant number.
data Instrs = Instrs Opcode Opcode Opcode Opcode
            | Jump3 Opcode Opcode Opcode Addr
            | Jump2 Opcode Opcode Addr
            | Jump1 Opcode Addr
            | Constant F18Word deriving (Eq)

instance Show Instrs where
  show (Instrs a b c d)   = unwords $ map show [a, b, c, d]
  show (Jump3 a b c addr) = unwords (map show [a, b, c]) ++ " " ++ show addr
  show (Jump2 a b addr)   = unwords (map show [a, b]) ++ " " ++ show addr
  show (Jump1 a addr)     = show a ++ " " ++ show addr
  show (Constant n)       = show n
  showList = (++) . unwords . map show

-- | A program in the F18A instruction set.
type NativeProgram = [Instrs]

-- | Returns the given instructions as an actual word. This assumes
-- the address is sized appropriately.
toBits :: Instrs -> F18Word
toBits (Instrs a b c d)   = fromOpcode a `shift` 13 .|. fromOpcode b `shift` 8  .|.
                            fromOpcode c `shift` 3  .|. fromOpcode d `shift` (-2)
toBits (Jump3 a b c addr) = fromOpcode a `shift` 13 .|. fromOpcode b `shift` 8  .|.
                            fromOpcode c `shift` 3  .|. addr
toBits (Jump2 a b addr)   = fromOpcode a `shift` 13 .|. fromOpcode b `shift` 8  .|. addr
toBits (Jump1 a addr)     = fromOpcode a `shift` 13 .|. addr
toBits (Constant n)       = n

-- | Reads in a word as a set of opcodes.
fromBits :: F18Word -> Instrs
fromBits n | isJump a  = Jump1 a     $ n .&. 0x3FF
           | isJump b  = Jump2 a b   $ n .&. 0xFF
           | isJump c  = Jump3 a b c $ n .&. 0x7
           | otherwise = Instrs a b c d
  where a = toOpcode $ n `shift` (-13)
        b = toOpcode $ n `shift` (-8) .&. 0x1F
        c = toOpcode $ n `shift` (-3) .&. 0x1F
        d = toOpcode $ (n .&. 0x7) `shift` 2

-- | Returns the opcodes in the given instruction word. A constant
-- corresponds to not having any opcodes.
toOpcodes :: Instrs -> [Opcode]
toOpcodes (Instrs a b c d) = [a, b, c, d]
toOpcodes (Jump3 a b c _)  = [a, b, c]
toOpcodes (Jump2 a b _)    = [a, b]
toOpcodes (Jump1 a _)      = [a]
toOpcodes Constant{}       = []

-- | Estimates the running time of the program in nanoseconds. This is
-- based on the numbers provided in the manual: faster instructions
-- take 1.5 nanoseconds and slower ones take 5. For now, this estimate
-- ignores control flow like ifs and loops.
runningTime :: NativeProgram -> Double
runningTime = sum . map opcodeTime . reverse . dropWhile (== Nop) . reverse . concatMap toOpcodes
