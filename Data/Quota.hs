module Data.Quota (applyQuota, Quota, Remainder) where

import qualified Data.Map as M
import Data.List

-- | A quota computes a 'RealFloat' from the total number of votes and the total
-- number of seats to partition
type Quota a = Integer -> Integer -> a

-- | A remainder sorts the parties following some rule using the number of votes remaining and 
-- the number of seats already allocated by each party
type Remainder a k = [(k,a,Integer)] -> [k]

applyQuo :: RealFloat a => Ord k =>
            Integer                         -- ^ number of seats to partition
         -> Quota a                         -- ^ method to use
         -> M.Map k Integer                 -- ^ map of votes
         -> (M.Map k Integer, M.Map k a)    -- ^ partition , votes left
applyQuo e q votes = (result, remainders)
    where
        parties = M.keys votes
        quota = q e (M.foldr (+) 0 votes)
        result = M.fromList [(k,floor (fromInteger (votes M.! k)/quota))| k<- parties]
        remainders = M.fromList [(k, fromInteger (votes M.! k) - fromInteger (result M.! k) * quota)| k<- parties]

applyRem :: RealFloat a => Ord k =>
            Integer           -- ^ number of seats to partition
         -> Remainder a k     -- ^ method to use
         -> M.Map k a         -- ^ map of votes remaining
         -> M.Map k Integer   -- ^ map of quota result
         -> M.Map k Integer   -- ^ partition
applyRem e r v a = if allocated == fromInteger e
                   then a
                   else applyRemAux a selected
    where
        allocated = M.foldr (+) 0 v
        parties = M.keys v
        lst = [(k, v M.! k, a M.! k)| k<-parties]
        selected = genericTake e (r lst)
        applyRemAux sol [] = sol 
        applyRemAux sol (x:xs) = applyRemAux (M.insertWith (+) x 1 sol) xs

-- | We allocate some seats given the 'Quota' method
-- and if there are seats left to allocate, we use the 'Remainder' method
applyQuota :: RealFloat a => Ord k =>
                  Integer              -- ^ number of seats to partition
               -> Quota a              -- ^ quota method to use
               -> Remainder a k        -- ^ remainder method to use
               -> M.Map k Integer      -- ^ map of votes
               -> M.Map k Integer      -- ^ partition
applyQuota e q r v = if allocated == fromInteger e
                     then allocation
                     else secondStep
    where
        (allocation,votes) = applyQuo e q v
        allocated = M.foldr (+) 0 v
        secondStep = applyRem e r votes allocation
