{-# LANGUAGE ExplicitForAll #-}
module Data.Quota (applyQuota, Quota, Remainder) where

import           Data.List
import qualified Data.Map   as M
import           Data.Votes

-- | A quota computes a 'RealFloat' from the total number of votes and the total
-- number of seats to partition
type Quota a = a -> Integer -> a

-- | A remainder sorts the parties following some rule using the number of votes remaining and
-- the number of seats already allocated by each party
type Remainder a k = [(k,a,Integer)] -> [k]

-- | Apply the first step of the quota method
applyQuo :: RealFloat a => Ord k =>
            Integer                         -- ^ number of seats to partition
         -> Quota a                         -- ^ method to use
         -> Votes k Integer                 -- ^ map of votes
         -> (Result k Integer, Votes k a)    -- ^ partition , votes left
applyQuo e q v = (Result result, Votes remainders)
    where
        votes = getVotes v
        parties = M.keys votes
        quota = q (M.foldr (\x y -> fromInteger x +y) 0 votes) e
        result = M.fromList [(k,floor (fromInteger (votes M.! k)/quota))| k<- parties]
        remainders = M.fromList [(k, fromInteger (votes M.! k) - fromInteger (result M.! k) * quota)| k<- parties]

-- | Apply the second step of the quota method
applyRem :: RealFloat a => Ord k =>
            Integer           -- ^ number of seats to partition
         -> Remainder a k     -- ^ method to use
         -> Votes k a         -- ^ map of votes remaining
         -> Result k Integer   -- ^ map of quota result
         -> Result k Integer   -- ^ partition
applyRem e r votes result = applyRemAux result selected
    where
        v = getVotes votes
        a= getResult result
        parties = M.keys v
        lst = [(k, v M.! k, a M.! k)| k<-parties]
        selected = genericTake e (r lst)
        applyRemAux sol []     = sol
        applyRemAux sol (x:xs) = applyRemAux (Result (M.insertWith (+) x 1 (getResult sol))) xs

-- | We allocate some seats given the 'Quota' method
-- and if there are seats left to allocate, we use the 'Remainder' method
applyQuota :: RealFloat a => Ord k =>
                  Integer              -- ^ number of seats to partition
               -> Quota a              -- ^ quota method to use
               -> Remainder a k        -- ^ remainder method to use
               -> Votes k Integer      -- ^ map of votes
               -> Result k Integer      -- ^ partition
applyQuota e q r v = if allocated == fromInteger e
                             then allocation
                             else secondStep
    where
        (allocation, votes) = applyQuo e q v
        allocated = M.foldr (+) 0 (getResult allocation)
        secondStep = applyRem (e - allocated) r votes allocation
