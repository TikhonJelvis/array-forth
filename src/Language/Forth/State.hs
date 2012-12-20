module Language.Forth.State where

import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V

import           Language.Forth.Instructions

type Memory = Vector F18Word

data State =
  State { a, b, p, i, r, s, t :: F18Word
        , ds, rs              :: Stack
        , memory              :: Memory  }
