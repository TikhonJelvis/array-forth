{-# LANGUAGE FlexibleInstances #-}
module Language.ArrayForth.Distance where

import           Data.Bits                       (Bits, popCount, xor)
import           Data.List                       (genericLength)
import           Data.Monoid                

import           Language.ArrayForth.Interpreter (Trace)
import           Language.ArrayForth.Opcode      (F18Word)
import           Language.ArrayForth.State

import           Language.Synthesis.Synthesis    (Score (..))

type Distance = Sum Double

instance Score Distance where toScore = getSum

-- | Counts the number of bits that differ between two numbers.
countBits :: (Integral n, Bits n) => n -> n -> Int
countBits n₁ n₂ = popCount $ (fromIntegral n₁ :: Int) `xor` fromIntegral n₂

-- | Return a distance function that counts the different bits between
-- the given registers. You could use it like `compareRegisters [s, t]`.
registers :: [State -> F18Word] -> (State -> State -> Distance)
registers regs s₁ s₂ = Sum . fromIntegral . sum $ zipWith countBits (go s₁) (go s₂)
  where go state = map ($ state) regs

-- | Returns a distance function that counts the different bits
-- between the given memory locations.
locations :: [F18Word] -> (State -> State -> Distance)
locations addresses s₁ s₂ = Sum . fromIntegral . sum $ zipWith countBits (go s₁) (go s₂)
  where go state = map (memory state !) addresses

-- | Returns a score that counts the number of matching states
-- according to some projection function.
matching :: Eq a => (State -> a) -> (Trace -> Trace -> Distance)
matching f t₁ t₂ = Sum $ -(genericLength t₂ - resultLength)
  where resultLength =  genericLength $ filter (`elem` map f t₁) (map f t₂)