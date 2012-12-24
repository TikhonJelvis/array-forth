module Language.Forth.Run where

import           Language.Forth.Interpreter
import           Language.Forth.Parse
import           Language.Forth.State

-- | Runs a given program from the default starting state.
runProgram :: String -> State
runProgram program = stepProgram $ setProgram 0 (parseProgram program) startState
