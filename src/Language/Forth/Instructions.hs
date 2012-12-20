module Language.Forth.Instructions where

import           Data.Word.Odd (Word18)

type F18Word = Word18

-- | A memory address, which is included in the various jump
--   instructions.
type Addr = Int

-- | A program in the F18A instruction set.
type F18A = [Opcode]

-- | Each F18A instruction, ordered by opcode.
data Opcode = Ret                -- ;
            | Exec               -- ex
            | Jump               -- name ;
            | Call               -- name
            | Unext              -- unext
            | Next               -- next
            | If                 -- if
            | NegIf              -- -if
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

-- | This type represents a word in memory. This word can either
--   contain opcodes, opcodes and an address or just a constant
--   number.
data MemWord = Instrs Opcode Opcode Opcode Opcode
             | Jump3 Opcode Opcode Opcode Addr
             | Jump2 Opcode Opcode Addr
             | Jump1 Opcode Addr
             | Constant F18Word
