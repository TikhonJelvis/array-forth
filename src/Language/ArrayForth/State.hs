{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
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

import           Data.Bits                         ((.&.), shiftR, testBit)
import           Data.Functor                      ((<$>))
import           Data.Vector.Unboxed               (Vector, (//))
import qualified Data.Vector.Unboxed               as V

import           Text.Printf                       (printf)

import           Language.ArrayForth.NativeProgram
import           Language.ArrayForth.Opcode        (F18Word)
import           Language.ArrayForth.Stack

  -- TODO: Figure out how to deal with different reads in ports.

-- | The four communication ports each core can use. These are
-- mirrored, so "up" actually means "down" on half the cores!
-- 
-- The order is based on the way the channels are addressed, from
-- least signficant bit *up*. This is the reverse of how they're
-- presented in the manual: we do uldr where the manual has rdlu.
data Port = U -- ^ up
          | L -- ^ left 
          | D -- ^ down
          | R -- ^ right
          deriving (Show, Eq, Bounded, Enum)

-- | An action on a communication port. 
data Action = Read F18Word | Write F18Word deriving (Show, Eq)

-- | Either read from or write to the ports specified by the memory
-- address. The ports are specified by four bits in the address.
--
-- Each port is controlled by one bit in the order @R@, @D@, @L@,
-- @U@. @D@ and @U@ are inverted—they are used if their control bit is
-- set to @0@.
--
-- This means the pattern to use all the ports is 1010; consequently,
-- using no ports is 0101. However, setting 0101 is really meaningless
-- and will map to not listening on *any* port, which is actually
-- represented with @Channel Nothing@.
setPort :: F18Word    -- ^ The address specifying which ports to use. Only
                     -- bits 5–8 matter, the rest are ignored.
           -> Action  -- ^ The action to perform on the port, either a
                     -- read or a write.
           -> Channel
setPort ports action = go $ (ports `shiftR` 4) .&. 0xF
  where go p | p == 0x5   = Channel Nothing
             | otherwise = Channel $ Just (filter enabled [U, D, L, R], action)
        enabled p = testBit ports $ fromEnum p + 5

-- | Read the input coming in on the given ports. 
readPort :: Channel -> Maybe F18Word
readPort (Channel word) = word >>= go
  where go (_, Read x)  = Just x
        go (_, Write _) = Nothing 

-- | A channel representing the four communication directions a core
-- may use. In practice, these will either be hooked up to other cores
-- or to IO. Nothing represents no message; if there is a word,
-- execution will block.
newtype Channel = Channel (Maybe ([Port], Action)) deriving (Show, Eq)

-- | An empty channel has no reads or writes and doesn't block execution.
emptyChannel :: Channel
emptyChannel = Channel Nothing

-- | The chip's RAM, ROM and IO channels. The RAM and ROM should each
-- contain 64 words.
data Memory = Memory { ram :: Vector Int
                     , rom :: Vector Int
                     , io  :: Channel } deriving (Show, Eq)

-- | Memory with RAM and ROM zeroed out and nothing on the
-- communication channels. 
emptyMem :: Memory
emptyMem = Memory { ram = V.replicate 64 0
                  , rom = V.replicate 64 0
                  , io  = emptyChannel }

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
                | otherwise       = readPort io

-- TODO: Fix how this deals with blocking!
-- | Set the memory using Forth words. Returns @Nothing@ if blocked on a write.
set :: State -> F18Word -> F18Word -> Maybe State
set state@State {memory = memory@Memory {..}} i value
  | i < 2 * memSize = Just updated
  | i < 4 * memSize = error "Cannot set memory in the ROM!"
  | otherwise       = if io == emptyChannel then Just updatedIO else Nothing
  where updated = state {
          memory = memory { ram = ram // [(toMem i, fromIntegral value)] }}
        updatedIO = state { memory = memory { io = setPort i (Write value) } }

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
