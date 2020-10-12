{-# LANGUAGE ExplicitForAll #-}
module Simulation.Simulation (Simulation, simulateDiv, simulateQuot) where

import           Data.Divisor
import qualified Data.Map     as M
import           Data.Quota
import           Data.Votes

-- | Given a list of elements, we provide a
-- map of votes given to those elements
type Simulation k = [k] -> Votes k Integer

simulateDiv :: RealFloat a => Ord k =>
               [k] ->
               Simulation k ->
               Divisor a ->
               Integer ->
               (Votes k Integer, Result k Integer)
simulateDiv lst s d e = (s lst, applyDiv e d (s lst))

simulateQuot :: RealFloat a => Ord k =>
                [k] ->
                Simulation k ->
                Quota a ->
                Remainder a k ->
                Integer ->
                (Votes k Integer, Result k Integer)
simulateQuot lst s q r e = (s lst, applyQuota e q r (s lst))
