{-# LANGUAGE ExplicitForAll #-}
module Data.Votes (Votes(Votes, getVotes), Result(Result, getResult), getVotesFromList) where

import           Data.List
import qualified Data.Map  as M

newtype Votes k a = Votes {getVotes :: M.Map k a}

newtype Result k a = Result {getResult :: M.Map k a}

getVotesFromList :: Num a => Ord k =>
                    M.Map [k] (Votes k a) ->
                    Votes k a
getVotesFromList m = Votes mm
    where
        mm = M.foldr (M.unionWith (+)) M.empty (M.map getVotes m)
