{-# LANGUAGE NamedFieldPuns #-}
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
runCPU :: CPU       -- ^ The start state
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
