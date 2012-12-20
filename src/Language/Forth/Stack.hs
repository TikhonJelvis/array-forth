module Language.Forth.Stack (empty, push, pop) where

import           Language.Forth.Instructions

newtype Stack = Stack [F18Word] deriving (Eq)

instance Show Stack where show (Stack body) = unwords $ map show body

-- | A stack containing only 0s.
empty :: Stack
empty = Stack $ replicate 8 0

-- | Pushes the given element on top of the stack, discarding the last element.
push :: F18Word -> Stack -> Stack
push word (Stack body) = Stack $ word : init body

-- | Pops the top of the stack, returning the value and the new stack.
pop :: Stack -> (Stack, F18Word)
pop (Stack (x:xs)) = (Stack $ xs ++ [x], x)
