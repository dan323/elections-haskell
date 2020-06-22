{-# LANGUAGE ExplicitForAll #-}
module Data.Quota.Provided(hare, droop, winnerAll, largestRemainder) where

import Data.Quota
import Data.List

hare :: RealFloat a => Quota a
hare votes seats = fromInteger votes / fromInteger seats

droop :: RealFloat a => Quota a
droop votes seats = (fromInteger votes / (fromInteger seats + 1)) + 1

winnerAll :: RealFloat a => Ord k => Remainder a k
winnerAll = repeat . first . maximumBy comparation

comparation :: Ord k => Ord a => (k,a,Integer) -> (k,a,Integer) -> Ordering
comparation (k1,x,_) (k2,y,_) = if x == y
                                then compare k1 k2
                                else compare y x

first :: (a,b,c) -> a
first (x,_,_) = x

largestRemainder :: RealFloat a => Ord k => Remainder a k
largestRemainder = fmap first . sortBy comparation