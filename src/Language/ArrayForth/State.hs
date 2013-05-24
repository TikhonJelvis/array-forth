{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
-- | This module defines types and functions for working with the
-- state of a single core.
--
-- The most important type is State, which contains all the
-- information about the core. This includes the registers, the
-- memory, both stacks and communication ports. Right now, it's just a
-- big record; in the future, I might make it more polymorphic using
-- lenses.
--
-- There are also some useful types and functions for working with the
-- memory of a chip and its communication channels.
module Language.ArrayForth.State where

import           Data.Functor                      ((<$>))
import           Data.Vector.Unboxed               (Vector, (//))
import qualified Data.Vector.Unboxed               as V

import           Text.Printf                       (printf)

import           Language.ArrayForth.Channel
import           Language.ArrayForth.NativeProgram
import           Language.ArrayForth.Opcode        (F18Word)
import           Language.ArrayForth.Stack

  -- TODO: Figure out how to deal with different reads in ports.

-- | The chip's RAM, ROM and IO channels. The RAM and ROM should each
-- contain 64 words.
--
-- For now, input and output is split into two different types, even
-- though they're combined on the physical chip. I'm simply not sure
-- how to handle the case that both chips simultaneously write to the
-- same channel.
data Memory = Memory { ram    :: Vector Int
                     , rom    :: Vector Int
                     , input  :: Channel
                     , output :: Channel } deriving (Show, Eq)

-- | Memory with RAM and ROM zeroed out and nothing on the
-- communication channels.
emptyMem :: Memory
emptyMem = Memory { ram    = V.replicate 64 0
                  , rom    = V.replicate 64 0
                  , input  = emptyChannel
                  , output = emptyChannel }

-- | The number of words in memory. Both ram and rom are this
-- size. For some reason, the ram and rom address spaces are *double*
-- this size respectively, wrapping around at the half-way point.
memSize :: Num a => a
memSize = 0x03F

-- | A state representing the registers, stacks, memory and
-- communication channels of a core. Note that all the fields are
-- strict; they should also be unboxed thanks to
-- @-funbox-strict-fields@ (set in the .cabal file).
--
-- For now, this is just a record; however, I might rewrite it to use
-- lenses in the near future.
data State =
  State { a, b, p, r, s, t       :: !F18Word
        , i                      :: !(Maybe F18Word)
          -- ^ the i register can be @Nothing@ if it is blocked on a
          -- communication port.
        , dataStack, returnStack :: !Stack
        , memory                 :: !Memory }

instance Show State where
  show State {p, a, b, r, s, t, dataStack} =
           printf "p:%s a:%s b:%s r:%s\n %s %s %s" p' a' b' r' t' s' (show dataStack)
    where [p', a', b', r', s', t'] = map show [p, a, b, r, s, t]

-- | The state corresponding to a core with no programs loaded and no
-- instructions executed.
startState :: State
startState = State 0 0 0 0 0 0 (Just 0) empty empty emptyMem

-- | Increment the p register for the given state. If p is in RAM or
-- ROM, this wraps p as appropriate. If p is in IO, this does nothing
-- and p remains unchanged.
incrP :: State -> State
incrP state@State { p } = state { p = nextP }
  where nextP | p < 2 * memSize = succ p `mod` 2 * memSize
              | p < 4 * memSize = succ p `mod` 2 * memSize + 2 * memSize
              | otherwise       = p

-- | The next word of instructions to execute in the given
-- state. Returns @Nothing@ if @p@ is blocked on a communication
-- channel.
next :: State -> Maybe Instrs
next State { memory, p } = fromBits <$> memory ! p

-- | Pops the data stack of the given state, updating @s@ and @t@.
dpop :: State -> (State, F18Word)
dpop state@State {s, t, dataStack} =
  let (ds', res) = pop dataStack in (state {t = s, s = res, dataStack = ds'}, t)

-- | Push a word onto the data stack, updating @s@ and @t@.
dpush :: State -> F18Word -> State
dpush state@State {s, t, dataStack} word =
  state {t = word, s = t, dataStack = push dataStack s}

-- | Pops the return stack of the given state, updating @r@.
rpop :: State -> (State, F18Word)
rpop state@State {r, returnStack} =
  let (rs', res) = pop returnStack in (state {r = res, returnStack = rs'}, r)

-- | Push a word onto the return stack, updating @r@.
rpush :: State -> F18Word -> State
rpush state@State {r, returnStack} word =
  state {r = word, returnStack = push returnStack r}

-- | Force an address to be in range of memory: [0,64), also
-- converting between different integral types.
toMem :: (Integral a, Integral b) => a -> b
toMem = fromIntegral . (`mod` 64)

-- | Read the memory at a location given by a Forth word. Returns
-- @Nothing@ if blocked on a communication channel.
(!) :: Memory -> F18Word -> Maybe F18Word
Memory {..} ! i | i < 2 * memSize = Just . fromIntegral $ ram V.! toMem i
                | i < 4 * memSize = Just . fromIntegral $ rom V.! toMem i
                | otherwise       = readPort i input

-- | Set the memory using Forth words. A state with anything in the
-- output channel remains blocked until one of the active ports is
-- read.
set :: State -> F18Word -> F18Word -> State
set state@State {memory = memory@Memory {..}} i value
  | i < 2 * memSize = state { memory = updatedRam }
  | i < 4 * memSize = error "Cannot set memory in the ROM!"
  | otherwise       = state { memory = updatedOutput }
  where updatedRam = memory { ram = ram // [(toMem i, fromIntegral value)] }
        updatedOutput = memory { output = writePort i value }

-- | Is the state is blocked because it has written to a port? Note
-- that this does *not* consider being blocked on a read!
blocked :: State -> Bool
blocked State { memory = Memory { output } } = output /= emptyChannel

-- | Loads the given program into memory at the given starting
-- position.
setProgram :: F18Word -> NativeProgram -> State -> State
setProgram start program state = state' { i = toBits <$> next state' }
  where state' = loadMemory start (fromIntegral . toBits <$> program) state

-- | Load the given memory words into the state starting at the given
-- address.
loadMemory :: F18Word -> [F18Word] -> State -> State
loadMemory start values state@State {memory = memory@Memory {..}} =
  state { memory = memory {
             ram = ram // zip [toMem start..] (fromIntegral <$> values) } }
