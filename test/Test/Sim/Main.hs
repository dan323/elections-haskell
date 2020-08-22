module Main where

import Data.Divisor
import Data.Divisor.Provided
import Data.Quota
import Data.Quota.Provided
import Simulation.Simulation
import Simulation.Provided

import Test.HUnit
import qualified Data.Map as M

limitTest :: Test
limitTest = TestCase $ assertEqual "MinMax limit" [] (filter (\x -> x<=1000 && x>=100) (M.elems (limitSim 2 100 1000 ["A", "B", "C"])))

main :: IO ()
main = do
  _ <- runTestTT limitTest
  return ()