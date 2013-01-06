{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.ArrayForth.NativeProgram where

import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad              ((<=<))

import           Data.Bits                  (shift, (.&.), (.|.))
import           Data.List.Split            (chunk, keepDelimsR, split, whenElt)
import           Data.String                (IsString, fromString)

import           Language.ArrayForth.Opcode
import           Language.ArrayForth.Parse

-- | Represents a word in memory. This word can either contain
-- opcodes, opcodes and a jump address or just a constant number.
data Instrs = Instrs Opcode Opcode Opcode Opcode
            | Jump3 Opcode Opcode Opcode F18Word
            | Jump2 Opcode Opcode F18Word
            | Jump1 Opcode F18Word
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

-- | Splits a list into chunks of at most four, breaking off a chunk
-- whenever it sees an element matching the given predicate. This is
-- useful for splitting a program along word boundaries, accounting
-- for jump addresses.
splitWords :: (a -> Bool) -> [a] -> [[a]]
splitWords isNum = chunk 4 <=< split (keepDelimsR $ whenElt isNum)

-- | Read a whole program, splitting instructions up into words.
readNativeProgram :: String -> Either ParseError NativeProgram
readNativeProgram = mapM go . splitWords isNumber . words
  where go [a, b, c, d] = do c' <- readOpcode c
                             if not $ isJump c'
                               then Instrs <$> op a <*> op b <*> op c <*> op3 d
                               else Jump3 <$> op a <*> op b <*> jump c <*> readWord d
        go [a, b, c]    = Jump2 <$> op a <*> jump b <*> readWord c
        go [a, b]       = Jump1 <$> jump a <*> readWord b
        go [a]          = Constant <$> readWord a
        go _            = error "Wrong number of instruction tokens!"
        wrap cond err str = do code <- readOpcode str
                               if cond code then Right code else Left $ err code
        op = wrap (not . isJump) $ NoAddr . show
        op3 = wrap slot3 $ NotSlot3 . show
        jump = wrap isJump $ NotJump . show

instance Read NativeProgram where
  readsPrec _ str = [(result, "")]
    where result = case readNativeProgram str of
            Right res -> res
            Left  err -> error $ show err

instance IsString NativeProgram where fromString = read

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
