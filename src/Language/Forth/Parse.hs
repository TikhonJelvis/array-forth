module Language.Forth.Parse where

import           Control.Applicative         ((<$>), (<|>))

import           Data.List                   (elemIndex)
import           Data.Maybe                  (listToMaybe, mapMaybe)

import           Language.Forth.Instructions

-- | The names of the different instructions, ordered by opcode.
names :: [String]
names = [";", "ex", "jump", "call", "unext", "next", "if", "-if", "@p", "@+", "@b", "@",
         "!p", "!+", "!b", "!", "+*", "2*", "2/", "-", "+", "and", "or", "drop", "dup",
         "pop", "over", "a", ".", "push", "b!", "a!"]

-- | Tries to read a given string as an opcode from the list of names.
readOpcode :: String -> Maybe Opcode
readOpcode token = toEnum <$> elemIndex token names

-- | Read a given string as a word's worth of instructions. This can
-- deal with addresses and numeric constants.
readInstrs :: String -> Maybe Instrs
readInstrs str = instrs <|> Constant <$> num str
  where num = fmap fst .  listToMaybe . reads
        ops = take 4 . mapMaybe readOpcode $ words str
        instrs = case ops of
          [a, b, c, d] -> Just $ Instrs a b c d
          [a, b, c]    -> Jump3 a b c <$> addr
          [a, b]       -> Jump2 a b <$> addr
          [a]          -> Jump1 a <$> addr
          _            -> Nothing
        addr = listToMaybe (reverse $ words str) >>= num
