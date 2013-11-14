{-# LANGUAGE NamedFieldPuns #-}

-- | This module extends the interpreter to model all 144 cores on a
-- chip. The basic idea is simple: we use the single-core interpreter
-- for a step and then update all the neighbor's communication ports.
--
-- Right now, we model one core at a time. The order in which cores
-- fire off is customizable. You control this order by passing in a
-- "schedule", which is just a list of core addresses; the cores are
-- then executed following this list.
--
-- For example, you could write a "round-robin" schedule using only
-- the first 10 cores as @cycle [Core 0 0..Core 0 9]@. Cores also work
-- like numbers, so you could use the following shorthand: @cycle
-- [0..9]@.
--
-- A @'CPU'@ is just a vector of 144 cores. To run a program, you first
-- have to load each core in the vector with its appropriate
-- program. You can do this using the @'//'@ operator which allows
-- bulk updates. For example, if you want to put states @s₁@, @s₂@ and
-- @s₃@ in addresses @[0..2]@, you could write:
-- 
-- @
-- base // [(0, s₁), (1, s₂), (2, s₃)]
-- -- or:
-- base // zip [0..] [s₁, s₂, s₃]
-- @
-- 
-- This creates a @'CPU'@ with all of the cores in their starting
-- configuration except for the explictly modified ones.
-- 
-- Once you have the starting configuration, you can run it with the
-- @'runCPU'@ function. This accepts the starting @'CPU'@ and a
-- schedule. So if you want to run the above state with a round-robin
-- schedule for the three active cores, you would do this:
-- 
-- @
-- runCPU (cycle [0..2]) (base // zip [0..] [s₁, s₂, s₃])
-- @
-- 
-- @'cycle'@ just repeats a given list forever. You can also define
-- your own schedules that are more interesting. For example, you
-- could write one where each core gets increasingly many steps at a
-- time:
-- 
-- @
-- [0..] >>= ([0,1,2] >>=) . replicate
-- @
-- 
-- (Here @'>>='@ for lists is just @'concatMap'@.)
--
-- More generally, you can use any list functions you want.
--
-- Another interesting thing would be to define a random schedule. You
-- can do this using @'Control.Monad.Random'@. The random generator
-- depends on a seed, so the easiest thing is to write a function that
-- generates a schedule given a seed:
--
-- @
-- randomSchedule seed = evalRand randomList $ mkStdGen seed
--   where randomList = do addr <- fromList . zip addresses $ repeat 1
--                         fmap (addr :) randomList
-- @
--
-- The @zip addresses $ repeat 1@ determines the possible addresses
-- and their weights. If you want some core to have a higher
-- probability of being chosen at each turn, just set its weight to
-- something other than 1.
module Language.ArrayForth.Multicore where

import qualified Data.Vector                     as V

import           Control.Applicative             ((<$>))
import           Control.Arrow                   (first)

import           Data.Maybe                      (catMaybes)

import           Language.ArrayForth.Channel
import           Language.ArrayForth.Core
import           Language.ArrayForth.Interpreter
import           Language.ArrayForth.State       (Memory (..), State (..))
import qualified Language.ArrayForth.State       as S

-- | The state of every core in the chip.
newtype CPU = CPU (V.Vector State)

-- | The state made up of all 144 cores in their start configurations.
base :: CPU
base = CPU $ V.replicate 144 S.startState

-- | We can index into the cores by core address.
(!) :: CPU -> Core -> State
CPU cpu ! core = cpu V.! fromEnum core

(//) :: CPU -> [(Core, State)] -> CPU
CPU cpu // updates = CPU $ cpu V.// map (first fromEnum) updates

-- | Runs every core according to the given schedule, starting from
-- the given initial state.
runCPU :: CPU    -- ^ The start state
       -> [Core] -- ^ The schedule. This is a list of core addresses to
                -- call. The execution trace will not be longer than
                -- the schedule.
       -> [CPU]  -- ^ The result is just a list of all the intermediate states.
runCPU cpu (core:cores) =
  let steps = stepCPU core cpu in steps ++ runCPU (last steps) cores

-- | Execute a single step of the simulation, running the specified
-- core. This executes a single word and returns all the intermediate
-- states for each instruction.
--
-- Since the schedule has to be discrete, communication only gets
-- propagated *after* the instructions all get executed. This is
-- probably bad, but meh.
stepCPU :: Core -> CPU -> [CPU]
stepCPU addr cpu = scanl (//) cpu $ steps : [(addr, end) : neighborUpdates addr end cpu]
  where states = stepAll $ cpu ! addr
        (steps, end) = (zip (repeat addr) $ init states, last states)

-- | Given an address, returns how to update neighbors with relevant
-- communication.
neighborUpdates :: Core -> State -> CPU -> [(Core, State)]
neighborUpdates addr state cpu = catMaybes cores `zip` catMaybes updated
  where Channel { right, down, left, up } = output $ memory state
        cores@[r, d, l, u] = neighbors addr
        updated = [go right R <$> r, go down D <$> d, go left L <$> l, go up U <$> u]

        go (Just value) port core = S.sendInput port value $ cpu ! core
        go Nothing _ core         = cpu ! core
