module Language.Forth.Stack (empty, push, pop, Stack) where

import           Language.Forth.Instructions

newtype Stack = Stack [F18Word] deriving (Eq)

instance Show Stack where show (Stack body) = unwords $ map show body

-- | A stack containing only 0s.
empty :: Stack
empty = Stack $ replicate 8 0

-- | Pushes the given element on top of the stack, discarding the last element.
push :: Stack -> F18Word -> Stack
push (Stack body) word = Stack $ word : init body

-- | Pops the top of the stack, returning the value and the new stack.
pop :: Stack -> (Stack, F18Word)
pop (Stack [])     = error "Empty stack."
pop (Stack (x:xs)) = (Stack $ xs ++ [x], x)
