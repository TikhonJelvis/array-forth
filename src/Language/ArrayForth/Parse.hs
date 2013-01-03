module Language.ArrayForth.Parse (ParseError (..), isNumber, readWord) where

import           Text.Printf                       (printf)

-- | Possible ways the input string can be malformed.
data ParseError = BadOpcode String
                | NotSlot3 String
                | NotJump String
                | NoAddr String
                | BadNumber String

instance Show ParseError where
  show (BadOpcode op) = printf "Invalid opcode `%s'." op
  show (NotSlot3 op)  = printf "`%s' cannot go into the last slot." op
  show (NotJump op)   =
    printf "`%s' is not a jump instruction and cannot get an address." op
  show (NoAddr op)    = printf "Missing a jump address for `%s'" op
  show (BadNumber n)  = printf "`%s' is not a valid number." n

-- | Is the given string a valid number with no other tokens?
isNumber :: String -> Bool
isNumber str = let asNumber = reads str :: [(Integer, String)] in
          not (null asNumber) && (null . snd $ head asNumber)

-- | Tries to read a word, giving an error if it fails.
readWord :: Read a => String -> Either ParseError a
readWord str = case reads str of
          (x, _) : _ -> Right x
          []         -> Left $ BadNumber str
