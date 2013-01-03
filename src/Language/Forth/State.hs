{-# LANGUAGE NamedFieldPuns #-}
module Language.Forth.State where

import           Data.Functor                 ((<$>))
import           Data.Vector.Unboxed          (Vector, (//))
import qualified Data.Vector.Unboxed          as V

import           Text.Printf                  (printf)

import           Language.Forth.NativeProgram
import           Language.Forth.Opcode        (F18Word)
import           Language.Forth.Stack

-- | The chip's RAM and ROM
type Memory = Vector Int

emptyMem :: Memory
emptyMem = V.replicate 64 0

-- | A state representing the registers, stacks and memory of a core.
data State =
  State { a, b, i, p, r, s, t    :: !F18Word
        , dataStack, returnStack :: !Stack
        , memory                 :: !Memory  }

instance Show State where
  show State {p, a, b, r, s, t, dataStack} =
           printf "p:%s a:%s b:%s r:%s\n %s %s %s" p' a' b' r' t' s' (show dataStack)
    where [p', a', b', r', s', t'] = map show [p, a, b, r, s, t]

-- | The state corresponding to a core with no programs loaded and no
-- instructions executed.
startState :: State
startState = State 0 0 0 0 0 0 0 empty empty emptyMem

-- | The next word of instructions to execute in the given state.
next :: State -> Instrs
next State {memory, p} = fromBits $ memory ! p

-- | Pops the data stack of the given state, updating s and t.
dpop :: State -> (State, F18Word)
dpop state@State {s, t, dataStack} =
  let (ds', res) = pop dataStack in (state {t = s, s = res, dataStack = ds'}, t)

-- | Push a word onto the data stack, updating s and t.
dpush :: State -> F18Word -> State
dpush state@State {s, t, dataStack} word =
  state {t = word, s = t, dataStack = push dataStack s}

-- | Pops the return stack of the given state, updating r.
rpop :: State -> (State, F18Word)
rpop state@State {r, returnStack} =
  let (rs', res) = pop returnStack in (state {r = res, returnStack = rs'}, r)

-- | Push a word onto the return stack, updating r.
rpush :: State -> F18Word -> State
rpush state@State {r, returnStack} word =
  state {r = word, returnStack = push returnStack r}

-- | Force an address to be in range of memory: [0,64).
toMem :: (Integral a, Integral b) => a -> b
toMem = fromIntegral . (`mod` 64)

-- | Read the memory at a location given by a Forth word.
(!) :: Memory -> F18Word -> F18Word
memory ! i | toMem i < V.length memory = fromIntegral $ memory V.! toMem i
           | otherwise                 = error "Memory out of bounds."

-- | Set the memory using Forth words.
set :: Memory -> F18Word -> F18Word -> Memory
set mem index value = mem // [(toMem index, fromIntegral $ value)]

-- | Loads the given program into memory at the given starting
-- position.
setProgram :: F18Word -> NativeProgram -> State -> State
setProgram start program state@State {memory} = state' {i = toBits $ next state'}
  where state' = state {memory = memory // prog}
        prog = zip [toMem start..] (fromIntegral . toBits <$> program)
