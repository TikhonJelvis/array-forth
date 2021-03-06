{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
-- | This module defines a type representing the location of a core in
-- the 8 × 18 grid.
-- 
-- All of the actually interesting code is in the typeclass instances.
module Language.ArrayForth.Core where

import           Data.Modular 

import           Text.Printf         (printf)

-- | The address of a core. There are 144 cores in an 8 × 18
-- array. The address has the row number followed by the column
-- number.
-- 
-- As a string, the core addresses are displayed as a single
-- three-digit number, just like in the GreenArray documentation. So
-- @Core 7 17@ becomes @\"717\"@.
-- 
-- Core addresses behave like numbers: you can use numeric literals
-- and add them together. For example, @[0..] :: [Core]@ gets you the
-- list of all the core addresses. @(move core = core + Core 1 1)@ is
-- a function that moves you up and over by one core.
data Core = Core !(ℤ/8) !(ℤ/18)

-- | Returns all the neighbors of a core. Most cores have four
-- neighbors; the ones along the edges only have three and the ones at
-- the corners two.
--
-- They always come in the order right, down, left up, with Nothing in
-- place of non-existant cores.
neighbors :: Core -> [Maybe Core]
neighbors core@(Core row col) = [ [ core + Core 1 0     | row /= maxBound ] 
                                , [ core + Core 0 1     | col /= maxBound ] 
                                , [ core + Core (-1) 0  | row /= minBound ] 
                                , [ core + Core 0 (- 1) | col /= minBound ] ]

-- Follows the same format as the documentation does: (7, 17) becomes 717.
instance Show Core where show (Core row col) = printf "%d%.2d" (unMod row) (unMod col)

deriving instance Eq Core
deriving instance Ord Core

instance Enum Core where
  fromEnum (Core r c) = fromInteger $ unMod r * 18 + unMod c
  toEnum n
    | n >= 0 && n < 144 = Core (toMod' $ n `div` 18) (toMod' $ n `mod` 18)
    | otherwise       = error "Core index out of bounds."

  -- Taken directly from the documentation for Enum:
  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where bound | fromEnum y >= fromEnum x = maxBound
	        | otherwise               = minBound

instance Bounded Core where
  minBound = Core 0 0
  maxBound = Core 7 17

-- Core addresses from a group, eh?
instance Num Core where
  fromInteger = toEnum . fromIntegral

  Core r₁ c₁ + Core r₂ c₂ = Core (r₁ + r₂) (c₁ + c₂)
  Core r₁ c₁ * Core r₂ c₂ = Core (r₁ * r₂) (c₁ * c₂)

  signum (Core r c) = Core (signum r) (signum c)
  abs    (Core r c) = Core (abs r) (abs c)
  negate (Core r c) = Core (negate r) (negate c)
