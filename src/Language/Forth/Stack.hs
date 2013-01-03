{-# LANGUAGE BangPatterns #-}
module Language.Forth.Stack (empty, push, pop, fill, Stack) where

import           Prelude                      hiding ((++))

import           Data.List                    (foldl')
import           Data.Vector.Unboxed          ((!), (++))
import qualified Data.Vector.Unboxed          as V

import           Language.Forth.Opcode        (F18Word)

newtype Stack = Stack (V.Vector Int) deriving (Eq)

instance Show Stack where show (Stack body) = unwords . map show $ V.toList body

-- | A stack containing only 0s.
empty :: Stack
empty = Stack $ V.replicate 8 0

-- | Pushes the given element on top of the stack, discarding the last element.
push :: Stack -> F18Word -> Stack
push !(Stack body) word = Stack . V.cons (fromIntegral word) $ V.init body

-- | Pops the top of the stack, returning the value and the new stack.
pop :: Stack -> (Stack, F18Word)
pop !(Stack body) = let x = V.take 1 body in (Stack $ V.tail body ++ x, fromIntegral $ x ! 0)

-- | Push the given elements onto the stack one-by-one.
fill ::  Stack -> [F18Word] -> Stack
fill = foldl' push

