{-# LANGUAGE NamedFieldPuns #-}
module Language.Forth.Interpreter where

import           Data.Bits                   (complement, shift, (.&.), xor, testBit)

import           Language.Forth.Instructions
import           Language.Forth.Stack
import           Language.Forth.State

size :: Int
size = 18

step :: Instrs -> State -> State
step (Instrs a b c d) state   = let s1 = execute a state
                                    s2 = if endWord a then s1 else execute b s1
                                    s3 = if endWord a || endWord b
                                         then s2 else execute c s2 in
                                if endWord a || endWord b || endWord c then s3 else execute d s3
step (Jump3 a b c addr) state = let s1 = execute a state
                                    s2 = if endWord a then s1 else execute b s1 in
                                if endWord a || endWord b then s2 else jump c addr s2
step (Jump2 a b addr) state   = let s' = execute a state in
                                if endWord a then s' else jump b addr s'
step (Jump1 a addr) state     = jump a addr state

-- | Does the given opcode cause the current word to stop executing?
endWord :: Opcode -> Bool
endWord = (`elem` [Ret, Exec, Jump, Call, Unext, Next, If, MinusIf])

execute :: Opcode -> State -> State
execute op state@(State {a, b, p, i, r, s, t, dataStack, returnStack, memory}) = case op of
  Ret          -> fst . rpop $ state {p = r}
  Exec         -> state {r = p, p = r}
  Unext        -> if r == 0 then fst $ rpop state else state {r = r - 1, p = p - 1}
  FetchP       -> dpush (state {p = p + 1}) $ memory ! p
  FetchPlus    -> dpush (state {a = a + 1}) $ memory ! a
  FetchB       -> dpush state $ memory ! b
  Fetch        -> dpush state $ memory ! a
  StoreP       -> let (s', res) = dpop state in s' {p = p + 1, memory = set memory p res}
  StorePlus    -> let (s', res) = dpop state in s' {a = a + 1, memory = set memory a res}
  StoreB       -> let (s', res) = dpop state in s' {memory = set memory b res}
  Store        -> let (s', res) = dpop state in s' {memory = set memory a res}
  MultiplyStep -> error "Not implemented yet!"
  Times2       -> state {t = t `shift` 1}
  Div2         -> state {t = t `shift` (-1)}
  Not          -> state {t = complement t}
  Plus         -> let (s', res) = pop dataStack in state {t = s + t, s = res, dataStack = s'}
  And          -> let (s', res) = pop dataStack in state {t = s .&. t, s = res, dataStack = s'}
  Or           -> let (s', res) = pop dataStack in state {t = s `xor` t, s = res, dataStack = s'}
  Drop         -> fst $ dpop state
  Dup          -> dpush state t
  Pop          -> let (s', res) = rpop state in dpush s' res
  Over         -> dpush state s
  ReadA        -> dpush state a
  Nop          -> state
  Push         -> let (s', res) = dpop state in rpush s' res
  SetB         -> let (s', res) = dpop state in s' {b = res}
  SetA         -> let (s', res) = dpop state in s' {a = res}
  _            -> error "Cannot jump without an address!"
  
jump :: Opcode -> Addr -> State -> State
jump op addr state@(State{p, r, t}) = case op of
  Jump    -> state {p = addr}
  Call    -> (rpush state p) {p = addr}
  Next    -> if r == 0 then fst $ rpop state else state {r = r - 1, p = addr}
  If      -> if t /= 0 then state {p = addr} else state
  MinusIf -> if t `testBit` pred size then state else state {p = addr}
  _       -> error "Non-jump instruction given a jump address!"