{-# LANGUAGE NamedFieldPuns #-}
module Language.Forth.Interpreter where

import           Data.Bits                   (bit, bitSize, complement, shift,
                                              testBit, xor, (.&.), (.|.))

import           Language.Forth.Instructions
import           Language.Forth.State

-- | Runs a single word's worth of instructions starting from the given state.
word :: Instrs -> State -> State
word (Instrs a b c d) state   = let s1 = execute a state
                                    s2 = if endWord a then s1 else execute b s1
                                    s3 = if endWord a || endWord b
                                         then s2 else execute c s2 in
                                if endWord a || endWord b || endWord c then s3 else execute d s3
word (Jump3 a b c addr) state = let s1 = execute a state
                                    s2 = if endWord a then s1 else execute b s1 in
                                if endWord a || endWord b then s2 else jump c addr s2
word (Jump2 a b addr) state   = let s' = execute a state in
                                if endWord a then s' else jump b addr s'
word (Jump1 a addr) state     = jump a addr state
word (Constant _) _           = error "Cannot execute a constant!"

-- | Executes a single instruction in the given state, incrementing
-- the program counter.
step :: State -> State
step state@State {p} = word (next state) $ state {p = p + 1, i = toBits $ next state}

-- | Returns a trace of the program's execution. The trace is a list
-- of the state of the chip after each step.
traceProgram :: State -> [State]
traceProgram = iterate step

-- | Trace a program until it either hits four nops or all 0s.
stepProgram :: State -> [State]
stepProgram = takeWhile (not . done) . traceProgram
  where done state = i state == 0x39ce7 || i state == 0

-- | Runs the program unil it hits a terminal state, returning only
-- the resulting state.
eval :: State -> State
eval start = case stepProgram start of
  []   -> start
  res  -> last res

-- | Executes the specified program on the given state until it hits a
-- "terminal" word--a word made up of four nops or all 0s.
runNativeProgram :: State -> NativeProgram -> State
runNativeProgram start program = eval $ setProgram 0 program start

-- | Steps the current state until it hits a terminal word,
-- calculating the time each opcode takes. This estimates the running
-- time for a particular trace of the program.
countTime :: State -> Double
countTime = runningTime . map (fromBits . i) . stepProgram

-- | Counts how many steps it takes to reach a terminal word. Each
-- step represents executing a single word's worth of instructions.
countSteps :: State -> Int
countSteps = length . stepProgram

-- | Runs the program, returning the result if it terminates in under
-- n steps and Nothing otherwise.
throttle :: Int -> State -> Maybe State
throttle steps state | null res || length res >= steps = Nothing
                     | otherwise                     = Just $ last res
  where res = stepProgram state

-- | Does the given opcode cause the current word to stop executing?
endWord :: Opcode -> Bool
endWord = (`elem` [Ret, Exec, Jump, Call, Unext, Next, If, MinusIf])

-- | Executes an opcode on the given state.
execute :: Opcode -> State -> State
execute op state@State {a, b, p, r, s, t, memory} = case op of
  Ret          -> fst . rpop $ state {p = r}
  Exec         -> state {r = p, p = r}
  Unext        -> if r == 0 then fst $ rpop state else state {r = r - 1, p = p - 1}
  FetchP       -> dpush (state {p = p + 1}) $ memory ! p
  FetchPlus    -> dpush (state {a = a + 1}) $ memory ! a
  FetchB       -> dpush state $ memory ! b
  Fetch        -> dpush state $ memory ! a
  StoreP       -> state' {p = p + 1, memory = set memory p top}
  StorePlus    -> state' {a = a + 1, memory = set memory a top}
  StoreB       -> state' {memory = set memory b top}
  Store        -> state' {memory = set memory a top}
  MultiplyStep -> multiplyStep
  Times2       -> state {t = t `shift` 1}
  Div2         -> state {t = t `shift` (-1)}
  Not          -> state {t = complement t}
  Plus         -> state' {t = s + t}
  And          -> state' {t = s .&. t}
  Or           -> state' {t = s `xor` t}
  Drop         -> fst $ dpop state
  Dup          -> dpush state t
  Pop          -> let (s', res) = rpop state in dpush s' res
  Over         -> dpush state s
  ReadA        -> dpush state a
  Nop          -> state
  Push         -> rpush state' top
  SetB         -> state' {b = top}
  SetA         -> state' {a = top}
  _            -> error "Cannot jump without an address!"
  where (state', top) = dpop state
        multiplyStep
          | even a    = let t0  = (t .&. 1) `shift` (bitSize t - 1) in
                        state { a = t0 .|. a `shift` (-1)
                              , t = t .&. bit 17 .|. t `shift` (-1)}
          | otherwise = let sum0 = (s + t) `shift` (bitSize t - 1)
                            sum17 = (s + t) .&. bit 17 in
                        state { a = sum0 .|. a `shift` (-1)
                              , t = sum17 .|. (s + t) `shift` (-1) }

-- | Execute a jump instruction to the given address.
jump :: Opcode -> Addr -> State -> State
jump op addr state@State{p, r, t} = case op of
  Jump    -> state {p = addr}
  Call    -> (rpush state p) {p = addr}
  Next    -> if r == 0 then fst $ rpop state else state {r = r - 1, p = addr}
  If      -> if t /= 0 then state {p = addr} else state
  MinusIf -> if t `testBit` pred (bitSize (0 :: F18Word)) then state else state {p = addr}
  _       -> error "Non-jump instruction given a jump address!"
