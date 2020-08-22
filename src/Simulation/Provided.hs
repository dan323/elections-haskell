{-# LANGUAGE ExplicitForAll #-}
module Simulation.Provided (limitSim, fixedSim) where

import           Simulation.Simulation

import qualified Data.Map              as M
import           System.Random

limitSim :: Ord k => Int -> Integer -> Integer -> Simulation k
limitSim gen min max lst = M.fromList (zip lst vals)
    where
        rnd  = mkStdGen gen
        vals = randomRs (min,max) rnd

fixedSim :: Ord k => (k -> Integer) -> Simulation k
fixedSim fun lst = M.fromList [(k,fun k)| k <- lst]
