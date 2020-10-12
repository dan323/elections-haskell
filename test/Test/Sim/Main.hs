module Main where

import Data.Divisor
import Data.Divisor.Provided
import Data.Quota
import Data.Quota.Provided
import Simulation.Simulation
import Simulation.Provided
import Data.Votes

import Test.HUnit
import qualified Data.Map as M

limitTest :: Test
limitTest = TestCase $ assertEqual "MinMax limit" 3 (length (filter (\x -> x<=1000 && x>=100) ((M.elems . getVotes) (limitSim 2 100 1000 ["A", "B", "C"]))))

sizeTest :: Test
sizeTest = TestCase $ assertEqual "Size" [] (filter (\x -> x>1000 || x<100) ((M.elems . getVotes) (limitSim 2 100 1000 ["A", "B", "C"])))

testlist :: Test
testlist = TestList [TestLabel "limit" limitTest,
                     TestLabel "full size" sizeTest      
                    ]

main :: IO ()
main = do
  _ <- runTestTT testlist
  return ()