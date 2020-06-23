{-# LANGUAGE ExplicitForAll #-}
module Data.Divisor (applyDiv, Divisor) where

import           Data.List
import qualified Data.Map  as M

-- | A divisor is a sctrictly increasing function $f : Nat -> Real$;
-- i.e. that n > m -> (f n) > (f m)
type Divisor a = Integer -> a

-- | This function is the main function that computes the solution to the partition
-- problem for a given 'Divisor' method
--
-- >>> applyDiv 4 fromInteger (M.fromList [("A",5),("B",2),("C",6),("D",7)])
-- M.fromList [("A",1),("B",1),("C",1),("D",1)]
-- 
-- >>> applyDiv 5 fromInteger (M.fromList [("BA",5),("B",5),("C",5),("D",5)])
-- M.fromList [("B",2),("BA",1),("C",1),("D",1)]
--
-- prop> sum (values (applyDiv n div votes)) == n
applyDiv :: RealFloat a => Ord k =>
            Integer                 -- ^ number of seats to partition
         -> Divisor a               -- ^ method to use
         -> M.Map k Integer         -- ^ map of votes
         -> M.Map k Integer         -- ^ partition
applyDiv e d votes = applyDivAux 0 votesInit M.empty
    where
        parties = M.keys votes
        applyDivAux act v sol
            | act == e = sol
            | otherwise = applyDivAux (act+1) updatedVotes updatedSol
            where
                winner = (minimum . map fst . filter (\(_,val) -> val == maxValue) . M.toList) v
                maxValue = (snd . maximumBy comp . M.toList) v
                updatedSol = M.insertWith (+) winner 1 sol
                updatedVotes = M.insert winner (fromInteger (votes M.! winner)/d (updatedSol M.! winner)) v
                comp (_,v1) (_,v2) = compare v1 v2
        votesInit = M.fromList [(k,fromInteger (votes M.! k) / d 0)| k<-parties]
