module Main where

import Data.Divisor
import Data.Divisor.Provided
import Data.Quota
import Data.Quota.Provided

import Test.HUnit
import qualified Data.Map as M

applyDivStDo :: Integer              -- ^ number of seats to partition
         -> Divisor Double           -- ^ method to use
         -> M.Map String Integer -- ^ map of votes
         -> M.Map String Integer
applyDivStDo = applyDiv

applyQuotaStDo :: Integer              -- ^ number of seats to partition
               -> Quota Double             -- ^ quota method to use
               -> Remainder Double String        -- ^ remainder method to use
               -> M.Map String Integer      -- ^ map of votes
               -> M.Map String Integer
applyQuotaStDo = applyQuota

testAdam :: Test
testAdam = TestCase $ assertEqual "Adam" (M.fromList [("A",1),("B",1)]) (applyDivStDo 2 adam (M.fromList [("A",100),("B",1)]))

testJefferson :: Test
testJefferson = TestCase $ assertEqual "Jefferson" (M.fromList [("A",11),("B",3),("C",1)]) (applyDivStDo 15 jefferson (M.fromList [("A",400),("B",120),("C",40)]))

testHamilton :: Test
testHamilton = TestCase $ assertEqual "Hamilton" (M.fromList [("A",4),("B",2)]) (applyQuotaStDo 6 hare largestRemainder (M.fromList [("A",400),("B",160),("C",40)]))

testlist :: Test
testlist = TestList [TestLabel "testAdam" testAdam,
                     TestLabel "testJefferson" testJefferson,
                     TestLabel "testHamilton" testHamilton         
                    ]

main :: IO ()
main = do
  _ <- runTestTT testlist
  return ()