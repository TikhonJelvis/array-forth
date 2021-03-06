module Language.ArrayForth.Opcode where

import           Data.List                 (elemIndex)
import           Data.Word.Odd             (Word18)

import           Language.ArrayForth.Parse (ParseError (..))

-- | The 18-bit word type used by Greenarrays chips.
type F18Word = Word18

-- | Each F18A instruction, ordered by opcode.
data Opcode = Ret                -- ;
            | Exec               -- ex
            | Jmp                -- name ;
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
            deriving (Eq, Bounded, Enum)

-- | The names of the different instructions, ordered by opcode.
names :: [String]
names = [";", "ex", "jump", "call", "unext", "next", "if", "-if", "@p", "@+", "@b", "@",
         "!p", "!+", "!b", "!", "+*", "2*", "2/", "-", "+", "and", "or", "drop", "dup",
         "pop", "over", "a", ".", "push", "b!", "a!"]

-- | All of the opcodes, in order.
opcodes :: [Opcode]
opcodes = [minBound..maxBound]

instance Show Opcode where show op = names !! fromEnum op

-- | Tries to read a given string as an opcode from the list of names.
readOpcode :: String -> Either ParseError Opcode
readOpcode token = case elemIndex token names of
  Just res -> Right $ toEnum res
  Nothing  -> Left  $ BadOpcode token

instance Read Opcode where readsPrec _ str = case readOpcode str of
                             Left err -> error $ show err
                             Right r  -> [(r, "")]

-- | Converts a word to an opcode. The word has to be < 32.
toOpcode :: F18Word -> Opcode
toOpcode = toEnum . fromIntegral

-- | Converts an Opcode to its 18-bit word representation.
fromOpcode :: Opcode -> F18Word
fromOpcode = fromIntegral . fromEnum

-- | Returns whether the given opcode is a jump instruction expecting
-- an address.
isJump :: Opcode -> Bool
isJump = (`elem` [Jmp, Call, Next, If, MinusIf])

-- | Can the given opcode go in the last slot?
slot3 :: Opcode -> Bool
slot3 = (`elem` [Ret, MultiplyStep, Unext, Plus, FetchP, Dup, StoreP, Nop])

-- | Estimates how long a given opcode will take to execute. Normal
-- opcodes take 1.5 nanoseconds where ones that access the memory take
-- 5 nanoseconds.
opcodeTime :: Opcode -> Double
opcodeTime op = if memoryOp op then 5 else 1.5
  where memoryOp = (`elem` [FetchP, FetchPlus, FetchB, Fetch, StoreP,
                            StorePlus, StoreB, Store])
