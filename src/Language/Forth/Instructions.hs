module Language.Forth.Instructions where

import           Data.Bits
import           Data.Word.Odd (Word18)

type F18Word = Word18

-- | A memory address, which is included in the various jump
-- instructions.
type Addr = F18Word

-- | A program in the F18A instruction set.
type Program = [Opcode]

-- | Each F18A instruction, ordered by opcode.
data Opcode = Ret                -- ;
            | Exec               -- ex
            | Jump               -- name ;
            | Call               -- name
            | Unext              -- unext
            | Next               -- next
            | If                 -- if
            | MinusIf            -- -if
            | FetchP             -- @p
            | FetchPlus          -- @+
            | FetchB             -- @b
            | Fetch              -- @
            | StoreP             -- !p
            | StorePlus          -- !+
            | StoreB             -- !b
            | Store              -- !
            | MultiplyStep       -- +*
            | Times2             -- 2*
            | Div2               -- 2/
            | Not                -- -
            | Plus               -- +
            | And                -- and
            | Or                 -- or
            | Drop               -- drop
            | Dup                -- dup
            | Pop                -- pop
            | Over               -- over
            | ReadA              -- a
            | Nop                -- .
            | Push               -- push
            | SetB               -- b!
            | SetA               -- a!
            deriving (Show, Eq, Bounded, Enum)

-- | Converts a word to an opcode. The word has to be < 32.
toOpcode :: F18Word -> Opcode
toOpcode = toEnum . fromIntegral

fromOpcode :: Opcode -> F18Word
fromOpcode = fromIntegral . fromEnum

-- | Returns whether the given opcode is a jump instruction expecting
-- an address.
isJump :: Opcode -> Bool
isJump = (`elem` [Jump, Call, Next, If, MinusIf])

-- | Represents a word in memory. This word can either contain
-- opcodes, opcodes and a jump address or just a constant number.
data MemWord = Instrs Opcode Opcode Opcode Opcode
             | Jump3 Opcode Opcode Opcode Addr
             | Jump2 Opcode Opcode Addr
             | Jump1 Opcode Addr
             | Constant F18Word deriving (Show, Eq)

-- | Returns the given instructions as an actual word. This assumes
-- the address is sized appropriately.
toBits :: MemWord -> F18Word
toBits (Instrs a b c d)   = fromOpcode a `shift` 13 .|. fromOpcode b `shift` 8  .|.
                            fromOpcode c `shift` 3  .|. fromOpcode d `shift` (-2)
toBits (Jump3 a b c addr) = fromOpcode a `shift` 13 .|. fromOpcode b `shift` 8  .|.
                            fromOpcode c `shift` 3  .|. addr
toBits (Jump2 a b addr)   = fromOpcode a `shift` 13 .|. fromOpcode b `shift` 8  .|. addr
toBits (Jump1 a addr)     = fromOpcode a `shift` 13 .|. addr
toBits (Constant n)       = n

-- | Reads in a word as a set of opcodes.
fromBits :: F18Word -> MemWord
fromBits n | isJump a  = Jump1 a     $ n .&. 0x3FF
           | isJump b  = Jump2 a b   $ n .&. 0xFF
           | isJump c  = Jump3 a b c $ n .&. 0x7
           | otherwise = Instrs a b c d
  where a = toOpcode $ n `shift` (-13)
        b = toOpcode $ n `shift` (-8) .&. 0x1F
        c = toOpcode $ n `shift` (-3) .&. 0x1F
        d = toOpcode $ (n .&. 0x7) `shift` 2
