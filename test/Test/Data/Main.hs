module Main where

import Data.Divisor
import Data.Divisor.Provided
import Data.Quota
import Data.Quota.Provided
import Data.Votes

import Test.HUnit
import qualified Data.Map as M

applyDivStDo :: Integer              -- ^ number of seats to partition
         -> Divisor Double           -- ^ method to use
         -> M.Map String Integer     -- ^ map of votes
         -> M.Map String Integer
applyDivStDo e d v = getResult (applyDiv e d (Votes v))

applyQuotaStDo :: Integer                   -- ^ number of seats to partition
               -> Quota Double              -- ^ quota method to use
               -> Remainder Double String   -- ^ remainder method to use
               -> M.Map String Integer      -- ^ map of votes
               -> M.Map String Integer
applyQuotaStDo e q r v = getResult (applyQuota e q r (Votes v))

testAdam :: Test
testAdam = TestCase $ assertEqual "Adam small" (M.fromList [("A",1),("B",1)]) (applyDivStDo 2 adam (M.fromList [("A",100),("B",1)]))

testAdam' :: Test
testAdam' = TestCase $ assertEqual "Adam" (M.fromList [("A",4),("B",1)]) (applyDivStDo 5 adam (M.fromList [("A",100),("B",1)]))

testJefferson :: Test
testJefferson = TestCase $ assertEqual "Jefferson" (M.fromList [("A",11),("B",3),("C",1)]) (applyDivStDo 15 jefferson (M.fromList [("A",400),("B",120),("C",40)]))

testHamilton :: Test
testHamilton = TestCase $ assertEqual "Hamilton" (M.fromList [("A",4),("B",2),("C",0)]) (applyQuotaStDo 6 hare largestRemainder (M.fromList [("A",400),("B",160),("C",40)]))

testlist :: Test
testlist = TestList [TestLabel "testAdam" testAdam,
                     TestLabel "testAdam 2" testAdam',
                     TestLabel "testJefferson" testJefferson,
                     TestLabel "testHamilton" testHamilton         
                    ]

main :: IO ()
main = do
  _ <- runTestTT testlist
  return ()