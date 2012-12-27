module Language.Forth.Distance where

import            Data.Bits                   (popCount, xor, Bits)

import            Language.Forth.Instructions
import            Language.Forth.State

-- | A function that computes a measure of "distance" between two
-- states. The larger the returned number, the more different the
-- states.
type Distance = State -> State -> Double

-- | Counts the number of bits that differ between two numbers.
countBits :: Bits n => n -> n -> Int
countBits n₁ n₂ = popCount $ n₁ `xor` n₂

-- | Return a distance function that counts the different bits between
-- the given registers. You could use it like `compareRegisters [s, t]`.
registers :: [State -> F18Word] -> Distance
registers regs s₁ s₂ = fromIntegral . sum $ zipWith countBits (go s₁) (go s₂)
  where go s = map ($ s) regs

-- | Returns a distance function that counts the different bits
-- between the given memory locations.
locations :: [F18Word] -> Distance
locations locations s₁ s₂ = fromIntegral . sum $ zipWith countBits (go s₁) (go s₂)
  where go s = map (memory s !) locations

-- | Combines multiple distance functions to create a new one, by
-- summing the different distances.
distances :: [Distance] -> Distance
distances dists s₁ s₂ = sum [dist s₁ s₂ | dist <- dists]
