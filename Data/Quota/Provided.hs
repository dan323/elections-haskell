module Data.Quota.Provided(hare,droop,winnerAll,largestRemainder) where

import Data.Quota
import Data.List

import qualified Data.Map as M

-- >>> applyQuota 6 hare largestRemainder (M.fromList [("A",400),("B",160),("C",40)])
-- fromList [("A",5),("B",1),("C",0)]
--
-- >>> applyRem 1 largestRemainder (M.fromList [("A",0.0),("B",60.0),("C",40.0)]) (M.fromList [("A",4),("B",1),("C",0)])
-- fromList [("A",5),("B",1),("C",0)]
--

hare :: RealFloat a => Quota a
hare votes seats = fromInteger votes / fromInteger seats

droop :: RealFloat a => Quota a
droop votes seats = (fromInteger votes / (fromInteger seats + 1)) + 1

winnerAll :: RealFloat a => Ord k => Remainder a k
winnerAll = repeat . first . maximumBy comparation

comparation :: Ord k => Ord a => (k,a,Integer) -> (k,a,Integer) -> Ordering
comparation (k1,x,_) (k2,y,_) = if x == y
                                then compare k1 k2
                                else compare x y

first :: (a,b,c) -> a
first (x,_,_) = x

largestRemainder :: RealFloat a => Ord k => Remainder a k
largestRemainder = fmap first . sortBy comparation