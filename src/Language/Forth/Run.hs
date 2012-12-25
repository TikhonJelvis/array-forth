module Language.Forth.Run where

import           Language.Forth.Instructions
import           Language.Forth.Interpreter
import           Language.Forth.Parse
import           Language.Forth.State
import           Language.Forth.Synthesis

-- | Runs a given program from the default starting state.
runProgram :: State -> String -> State
runProgram start = runNativeProgram start . parseProgram

-- | Runs the given program in the synthesizer's format on the default
-- starting state.
runInstructions :: State -> Program -> State
runInstructions start = runNativeProgram start . toNative

-- | Runs the given program in the native format on the default
-- starting state.
runNativeProgram :: State -> NativeProgram -> State
runNativeProgram start program = stepProgram $ setProgram 0 program start
