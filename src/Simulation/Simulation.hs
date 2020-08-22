{-# LANGUAGE ExplicitForAll #-}
module Simulation.Simulation (Simulation, simulateDiv, simulateQuot) where

import           Data.Divisor
import           Data.Quota
import qualified Data.Map as M

type Simulation k = [k] -> M.Map k Integer

simulateDiv :: RealFloat a => Ord k =>
               [k] ->
               Simulation k ->
               Divisor a ->
               Integer ->
               (M.Map k Integer, M.Map k Integer)
simulateDiv lst s d e = (s lst, applyDiv e d (s lst))

simulateQuot :: RealFloat a => Ord k =>
                [k] ->
                Simulation k ->
                Quota a ->
                Remainder a k ->
                Integer -> 
                (M.Map k Integer, M.Map k Integer)
simulateQuot lst s q r e = (s lst, applyQuota e q r (s lst))
