{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Data.Functor                    ((<$))
import           Data.List.Split                 (chunk)
import qualified Data.Vector.Unboxed             as V

import           Language.ArrayForth.Interpreter (eval, runNativeProgram)
import           Language.ArrayForth.Parse       (isNumber)
import           Language.ArrayForth.Program     (toNative, readProgram)
import           Language.ArrayForth.State       (State (..), setProgram,
                                                  startState)

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
repl = go 0 startState
  where go loc state =
          do inp <- putStr "λ>" >> hFlush stdout >> getLine
             case inp of
               ":"            -> putStrLn "Please specify a command!" >> go loc state
               ':' : commands -> let command : args = words commands in
                 run command args >>= uncurry go
               program        -> execute $ readProgram program
          where execute (Left err)      = print err >> go loc state
                execute (Right program) = print res >> go p res
                  where res@State {p} = eval $ setProgram loc (toNative program) state
                run "reset" _   = (0, startState) <$ print startState
                run "p" args
                  | null args || not (isNumber $ head args) =
                    (loc, state) <$ putStrLn "Invalid arguments!"
                  | otherwise = let n = read $ head args in
                    (n, state { p = n }) <$ print state { p = n }
                run cmd args  = (loc, state) <$ continue cmd args
                continue "memory" _ = mapM_ print . chunk 8 . V.toList $ memory state
                continue cmd _      = printf "Unknown command `%s'!\n" cmd
