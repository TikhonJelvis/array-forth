{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
module Language.ArrayForth.Interpreter where

import           Data.Bits
import           Data.Functor                      ((<$>))
import           Data.Maybe                        (fromJust, fromMaybe, mapMaybe)

import           Language.ArrayForth.NativeProgram
import           Language.ArrayForth.Opcode
import           Language.ArrayForth.State

-- | A trace of a progam is the state after every word is executed.
type Trace = [State]

-- | Runs a single word's worth of instructions starting from the
-- given state, returning the intermediate states for each executed
-- opcode.
wordAll :: Instrs -> State -> [State]
wordAll (Instrs a b c d) state   =
  let s₁ = [execute a state]
      s₂ = if endWord a then s₁ else run b s₁
      s₃ = if endWord a || endWord b
           then s₂ else run c s₂ in
  if endWord a || endWord b || endWord c then s₃ else s₃ ++ run d s₃
wordAll (Jump3 a b c addr) state = let s₁ = [execute a state]
                                       s₂ = if endWord a then s₁ else run b s₁ in
                                   if endWord a || endWord b
                                     then s₂ else s₂ ++ [jump c addr (last s₂)]
wordAll (Jump2 a b addr) state   = let s' = execute a state in
                                   if endWord a then [s'] else [s', jump b addr s']
wordAll (Jump1 a addr) state     = [jump a addr state]
wordAll (Constant _) _           = error "Cannot execute a constant!"

-- | Runs a single word's worth of instructions, returning only the
-- final state.
word :: Instrs -> State -> State
word instr σ = last $ wordAll instr σ

-- | Executes a single word in the given state, incrementing
-- the program counter and returning all the intermediate states.
stepAll :: State -> [State]
stepAll state = fromMaybe [] $ go <$> next state
  where go instrs = wordAll instrs . incrP $ state {i = toBits <$> next state}

-- | Executes a single word in the given state, returning the last
-- resulting state.q
step :: State -> State
step = last . stepAll

-- | Trace the given program, including all the intermediate states.
traceAll :: State -> Trace
traceAll program = let steps = stepAll program in steps ++ traceAll (last steps)

-- | Returns a trace of the program's execution. The trace is a list
-- of the state of the chip after each step.
traceProgram :: State -> Trace
traceProgram = iterate step

-- | Trace a program until it either hits four nops or all 0s.
stepProgram :: State -> Trace
stepProgram = takeWhile (not . done) . traceProgram
  where done state = i state == Just 0x39ce7 || i state == Just 0

-- | Runs the program unil it hits a terminal state, returning only
-- the resulting state.
eval :: State -> State
eval state = last $ state : stepProgram state

-- | Executes the specified program on the given state until it hits a
-- "terminal" word--a word made up of four nops or all 0s.
runNativeProgram :: State -> NativeProgram -> State
runNativeProgram start program = eval $ setProgram 0 program start

-- | Estimates the execution time of a program trace.
countTime :: Trace -> Double
countTime = runningTime . mapMaybe (fmap fromBits . i)

-- | Checks that the program trace terminated in at most n steps,
-- returning Nothing otherwise.
throttle :: Int -> Trace -> Either Trace Trace
throttle n states | null res       = Right [startState]
                  | length res == n = Left res
                  | otherwise      = Right res
  where res = take n states

-- | Does the given opcode cause the current word to stop executing?
endWord :: Opcode -> Bool
endWord = (`elem` [Ret, Exec, Jmp, Call, Unext, Next, If, MinusIf])

-- | Extends the given trace by a single execution step. The trace
-- cannot be empty.
run :: Opcode -> [State] -> [State]
run op trace = trace ++ [execute op $ last trace]

-- | Executes an opcode on the given state. If the state is blocked on
-- some communication, nothing changes.
execute :: Opcode -> State -> State
execute op state@State {..} = fromMaybe state [ res | res <- result, not $ blocked res ]
  where result = case op of
          FetchP       -> dpush (incrP state) <$> memory ! p
          FetchPlus    -> dpush (state {a = a + 1}) <$> memory ! a
          FetchB       -> dpush state <$> memory ! b
          Fetch        -> dpush state <$> memory ! a
          _            -> Just normal
        normal = case op of
          Ret          -> fst . rpop $ state {p = r}
          Exec         -> state {r = p, p = r}
          Unext        -> if r == 0 then fst $ rpop state
                                  else state {r = r - 1, p = p - 1}
          StoreP       -> incrP $ set state' p top
          StorePlus    -> set (state' { a = a + 1 }) a top
          StoreB       -> set state' b top
          Store        -> set state' a top
          MultiplyStep -> multiplyStep
          Times2       -> state {t = t `shift` 1}
          Div2         -> state {t = t `shift` (-1)}
          Not          -> state {t = complement t}
          Plus         -> state' {t = s + t}
          And          -> state' {t = s .&. t}
          Or           -> state' {t = s `xor` t}
          Drop         -> fst $ dpop state
          Dup          -> dpush state t
          Pop          -> uncurry dpush $ rpop state
          Over         -> dpush state s
          ReadA        -> dpush state a
          Nop          -> state
          Push         -> rpush state' top
          SetB         -> state' {b = top}

          SetA         -> state' {a = top}
          _            -> error "Cannot jump without an address!"

        (state', top) = dpop state
        -- TODO: support different word sizes?
        multiplyStep
          | even a    = let t0  = (t .&. 1) `shift` (size - 1) in
                        state { a = t0 .|. a `shift` (-1)
                              , t = t .&. bit (size - 1) .|. t `shift` (-1)}
          | otherwise = let sum0 = (s + t) `shift` (size - 1)
                            sum17 = (s + t) .&. bit (size - 1) in
                        state { a = sum0 .|. a `shift` (-1)
                              , t = sum17 .|. (s + t) `shift` (-1) }
        size = bitSize t

-- | Execute a jump instruction to the given address.
jump :: Opcode -> F18Word -> State -> State
jump op addr state@State{p, r, t} = case op of
  Jmp     -> state {p = addr}
  Call    -> (rpush state p) {p = addr}
  Next    -> if r == 0 then fst $ rpop state else state {r = r - 1, p = addr}
  If      -> if t /= 0 then state {p = addr} else state
  MinusIf -> if t `testBit` pred size then state else state {p = addr}
  _       -> error "Non-jump instruction given a jump address!"
  where size = bitSize (0 :: F18Word)
