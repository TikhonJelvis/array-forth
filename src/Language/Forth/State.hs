{-# LANGUAGE NamedFieldPuns #-}
module Language.Forth.State where

import           Data.Vector                 (Vector, (//))
import qualified Data.Vector                 as V

import           Language.Forth.Instructions
import           Language.Forth.Stack

-- | The chip's RAM and ROM
type Memory = Vector F18Word

-- | A state representing the registers, stacks and memory of a core.
data State =
  State { a, b, p, r, s, t       :: F18Word
        , dataStack, returnStack :: Stack
        , memory                 :: Memory  }

-- | Pops the data stack of the given state, updating s and t.
dpop :: State -> (State, F18Word)
dpop state@(State {s, t, dataStack}) =
  let (ds', res) = pop dataStack in (state {t = s, s = res, dataStack = ds'}, t)

-- | Push a word onto the data stack, updating s and t.
dpush :: State -> F18Word -> State
dpush state@(State {t, dataStack}) word =
  state {t = word, s = t, dataStack = push dataStack word}

-- | Pops the return stack of the given state, updating r.
rpop :: State -> (State, F18Word)
rpop state@(State {r, returnStack}) =
  let (rs', res) = pop returnStack in (state {r = res, returnStack = rs'}, r)

-- | Push a word onto the return stack, updating r.
rpush :: State -> F18Word -> State
rpush state@(State {returnStack}) word =
  state {r = word, returnStack = push returnStack word}

-- | Read the memory at a location given by a Forth word.
(!) :: Memory -> F18Word -> F18Word
memory ! index = memory V.! (fromIntegral index)

-- | Set the memory using Forth words.
set :: Memory -> F18Word -> F18Word -> Memory
set mem index value = mem // [(fromIntegral index, value)]