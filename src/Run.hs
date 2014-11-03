{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Data.Functor                    ((<$))
import           Data.List.Split                 (chunk)
import qualified Data.Vector.Unboxed             as V

import           Language.ArrayForth.Interpreter (eval, runNativeProgram)
import           Language.ArrayForth.Parse       (isNumber)
import           Language.ArrayForth.Program     (readProgram, toNative)
import           Language.ArrayForth.State       (Memory (..), State (..),
                                                  setProgram, startState)

import           System.Environment              (getArgs)
import           System.IO                       (hFlush, stdout)

import           Text.Printf                     (printf)

main :: IO ()
main = do args <- getArgs
          case args of
            []     -> repl
            [file] -> readFile file >>= print . runNativeProgram startState . read
            _      -> putStrLn $ "Too many arguments!"

repl :: IO ()
repl = putStrLn errorMessage >> go (0, startState)
  where errorMessage = "Type :help for a list of possible command."
                
        go (loc, state) = do
          inp <- putStr "λ>" >> hFlush stdout >> getLine
          case inp of
            ":"            -> do
              putStrLn ("Please specify a valid command. " ++ errorMessage)
              go (loc, state)
            ':' : commands -> let command : args = words commands in
              run command args >>= go
            program        -> execute $ readProgram program
          where helpMessage = unlines $ [
                  ":help   — list the possible commands",
                  ":reset  — reset all the registers and memory to 0",
                  ":p      — print the value of the p register (the program counter)",
                  ":p <n>  — set the p register to the given address n; a manual jump instruction",
                  ":memory — print all of the memory in a reasonably easy to read format"]
                
                execute (Left err)      = print err >> go (loc, state)
                execute (Right program) = print res >> go (p, res)
                  where res@State {p} = eval $ setProgram loc (toNative program) state
                run "reset" _   = (0, startState) <$ print startState
                run "p" []      = (loc, state) <$ print (p state)
                run "p" args
                  | not (isNumber $ head args) =
                    (loc, state) <$ putStrLn "Invalid arguments!"
                  | otherwise = let n = read $ head args in
                    (n, state { p = n }) <$ print state { p = n }
                run cmd args  = (loc, state) <$ continue cmd args
                continue "help" _   = putStr helpMessage
                continue "memory" _ = mapM_ print . chunk 8 . V.toList . ram $ memory state
                continue cmd _      = printf "Unknown command `%s'!\n%s" cmd errorMessage
