module Language.Forth.Synthesis where

import           Data.List.Split             (chunk)

import           Language.Forth.Instructions

-- | Represents a single instruction as viewed by the
-- synthesizer. This can be an opcode, a numeric literal or a token
-- representing an unused slot.
data Instruction = Opcode Opcode
                 | Number F18Word
                 | Unused deriving (Show, Eq)

-- | A program to be manipulated by the MCMC synthesizer
type Program = [Instruction]

-- | Takes a program as handled by the synthesizer and makes it native
-- by turning literal numbers into @p and fixing any issues with
-- instructions going into the last slot.
toNative :: Program -> NativeProgram
toNative = concatMap (toInstrs . addFetchP) . chunk 4 . fixSlot3 . filter (/= Unused)
  where addFetchP ops = uncurry (++) (go ops)
          where go [] = ([], [])
                go (n@Number{} : rest) = let (instrs, consts) = go rest in
                  (Opcode FetchP : instrs, n : consts)
                go (instr : rest) = let (instrs, consts) = go rest in
                  (instr : instrs, consts)
        toInstrs (Opcode a : Opcode b : Opcode c : Opcode d : rest) =
          Instrs a b c d : map (\ (Number n) -> Constant n) rest
        toInstrs ls = toInstrs . take 4 $ ls ++ repeat (Opcode Nop)

-- | Take a program and ensure that only instructions allowed in the
-- last slot go there by adding nops as necessary.
fixSlot3 :: Program -> Program
fixSlot3 program
  | length program < 4 = program
  | slot3 op4          = take 4 program ++ fixSlot3 (drop 4 program)
  | otherwise          = take 3 program ++ [Opcode Nop] ++ fixSlot3 (drop 3 program)
  where op4 = case program !! 3 of
          Opcode op -> op
          Number{}  -> FetchP
          Unused    -> error "Cannot have unused slot in fixSlot3!"

-- | Returns a measure of the quality of the program. For now this is
-- only based on the performance of the program.
evaluate :: Program -> Double
evaluate = negate . runningTime . toNative