{-# LANGUAGE ExplicitForAll #-}
module Data.Quota.Provided(hare, droop, winnerAll, largestRemainder) where

import Data.Quota
import Data.List

-- | The most simple quota, votes by seat
hare :: RealFloat a => Quota a
hare votes seats = votes / fromInteger seats

-- | A most involved quota is one where we take a lower result division
-- and ask for one more vote
droop :: RealFloat a => Quota a
droop votes seats = (votes / (fromInteger seats + 1)) + 1

-- | The element with the biggest remainder, takes all left
winnerAll :: RealFloat a => Ord k => Remainder a k
winnerAll = repeat . first . maximumBy comparation

comparation :: Ord k => Ord a => (k,a,Integer) -> (k,a,Integer) -> Ordering
comparation (k1,x,_) (k2,y,_) = if x == y
                                then compare k1 k2
                                else compare y x

first :: (a,b,c) -> a
first (x,_,_) = x

-- | The largest remainders take the seats left
largestRemainder :: RealFloat a => Ord k => Remainder a k
largestRemainder = fmap first . sortBy comparation