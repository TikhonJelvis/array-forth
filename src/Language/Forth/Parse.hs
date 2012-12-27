module Language.Forth.Parse (readOpcode, readProgram, ParseError (..),
                             parseProgram, displayProgram) where

import           Control.Applicative         ((<$>), (<*>))

import           Data.List                   (elemIndex)
import           Data.List.Split             (chunk, keepDelimsR, split,
                                              whenElt)

import           Text.Printf                 (printf)

import           Language.Forth.Instructions

-- | Possible ways the input string can be malformed.
data ParseError = BadOpcode String
                | NotSlot3 Opcode
                | NotJump Opcode
                | NoAddr Opcode
                | BadNumber String

instance Show ParseError where
  show (BadOpcode op) = printf "Invalid opcode `%s'." op
  show (NotSlot3 op)  = printf "`%s' cannot go into the last slot." $ show op
  show (NotJump op)   =
    printf "`%s' is not a jump instruction and cannot get an address." $ show op
  show (NoAddr op)    = printf "Missing a jump address for `%s'" $ show op
  show (BadNumber n)  = printf "`%s' is not a valid number." n

-- | Tries to read a given string as an opcode from the list of names.
readOpcode :: String -> Either ParseError Opcode
readOpcode token = case elemIndex token names of
  Just res -> Right $ toEnum res
  Nothing  -> Left  $ BadOpcode token

-- | Is the given string a valid number with no other tokens?
isNumber :: String -> Bool
isNumber str = let asNumber = reads str :: [(Integer, String)] in
          not (null asNumber) && (null . snd $ head asNumber)

-- | Read a whole program, splitting instructions up into words.
readProgram :: String -> Either ParseError [Instrs]
readProgram = mapM go . separate . words
  where separate = concatMap (chunk 4) . split (keepDelimsR $ whenElt isNumber)
        go [a, b, c, d] = do c' <- readOpcode c
                             if not $ isJump c'
                               then Instrs <$> op a <*> op b <*> op c <*> op3 d
                               else Jump3 <$> op a <*> op b <*> jump c <*> num d
        go [a, b, c]    = Jump2 <$> op a <*> jump b <*> num c
        go [a, b]       = Jump1 <$> jump a <*> num b
        go [a]          = Constant <$> num a
        go _            = error "Wrong number of instruction tokens!"
        num str = case reads str of
          (x, _) : _ -> Right x
          []         -> Left $ BadNumber str
        wrap cond err str = do code <- readOpcode str
                               if cond code then Right code else Left $ err code
        op = wrap (not . isJump) NoAddr
        op3 = wrap slot3 NotSlot3
        jump = wrap isJump NotJump

-- | Reads a program, calling error if the parse fails.
parseProgram :: String -> NativeProgram
parseProgram program = case readProgram program of
  Right res -> res
  Left  err -> error $ show err

-- | Displays an instruction word as opcodes and numbers.
displayInstrs :: Instrs -> String
displayInstrs (Instrs a b c d)   = unwords $ map show [a, b, c, d]
displayInstrs (Jump3 a b c addr) = unwords (map show [a, b, c]) ++ " " ++ show addr
displayInstrs (Jump2 a b addr)   = unwords (map show [a, b]) ++ " " ++ show addr
displayInstrs (Jump1 a addr)     = show a ++ " " ++ show addr
displayInstrs (Constant n)       = show n

-- | Displays a whole program as a runnable string of instructions.
displayProgram :: NativeProgram -> String
displayProgram = unwords . map displayInstrs
