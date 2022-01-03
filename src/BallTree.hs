module BallTree where

import qualified Data.Set as S

import Domain
import Utils
import BallTree.Search
import qualified BallTree.CircleTree.SingletonLeaf as BT

type BallTree a = BT.BallTree a

ballTree :: Foldable m => Metric a -> SearchAlgorithm -> m a -> BT.BallTree a
ballTree = BT.buildBallTree

getNeighbors :: Ord a => Metric a -> a -> SearchAlgorithm -> BallTree a -> S.Set a
{-# INLINE getNeighbors #-}
{-# SPECIALIZE getNeighbors :: Metric (WithOffset a) -> WithOffset a -> SearchAlgorithm -> BallTree (WithOffset a) -> S.Set (WithOffset a) #-}
getNeighbors d p (BallSearch radius) = BT.ballSearch d p radius
getNeighbors d p (KnnSearch k) = undefined 
