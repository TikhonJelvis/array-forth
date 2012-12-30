{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Forth.Parse (readOpcode, readProgram, readNativeProgram, ParseError (..),
                             isNumber) where

import           Control.Applicative         ((<$>), (<*>))

import           Data.List                   (elemIndex)
import           Data.List.Split             (chunk, keepDelimsR, split,
                                              whenElt)

import           Text.Printf                 (printf)

import           Language.Forth.Instructions
import           Language.Forth.Synthesis

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

instance Read Opcode where readsPrec _ str = case readOpcode str of
                             Left err -> error $ show err
                             Right r  -> [(r, "")]

-- | Is the given string a valid number with no other tokens?
isNumber :: String -> Bool
isNumber str = let asNumber = reads str :: [(Integer, String)] in
          not (null asNumber) && (null . snd $ head asNumber)

-- | Reads a Forth word as a numeric literal.
readWord :: String -> Either ParseError F18Word
readWord str = case reads str of
          (x, _) : _ -> Right x
          []         -> Left $ BadNumber str

-- | Read a whole program, splitting instructions up into words.
readNativeProgram :: String -> Either ParseError NativeProgram
readNativeProgram = mapM go . separate . words
  where separate = concatMap (chunk 4) . split (keepDelimsR $ whenElt isNumber)
        go [a, b, c, d] = do c' <- readOpcode c
                             if not $ isJump c'
                               then Instrs <$> op a <*> op b <*> op c <*> op3 d
                               else Jump3 <$> op a <*> op b <*> jump c <*> readWord d
        go [a, b, c]    = Jump2 <$> op a <*> jump b <*> readWord c
        go [a, b]       = Jump1 <$> jump a <*> readWord b
        go [a]          = Constant <$> readWord a
        go _            = error "Wrong number of instruction tokens!"
        wrap cond err str = do code <- readOpcode str
                               if cond code then Right code else Left $ err code
        op = wrap (not . isJump) NoAddr
        op3 = wrap slot3 NotSlot3
        jump = wrap isJump NotJump

instance Read NativeProgram where
  readsPrec _ str = [(result, "")]
    where result = case readNativeProgram str of
            Right res -> res
            Left  err -> error $ show err

-- | Tries to parse the given string as an instruction, which can
-- either be a number, an opcode or "_" representing Unused.
readInstruction :: String -> Either ParseError Instruction
readInstruction "_"                = Right Unused
readInstruction str | isNumber str = Number <$> readWord str
                    | otherwise    = Opcode <$> readOpcode str

-- | Reads a program in the synthesizer's format.
readProgram :: String -> Either ParseError Program
readProgram = mapM readInstruction . words

instance Read Program where
  readsPrec _ str = [(result, "")]
    where result = case readProgram str of
            Right res -> res
            Left  err -> error $ show err
