module BallTree where

import qualified Data.Set as S
import qualified Data.Vector as V

import Domain
import qualified BallTree.Internal as BT
import Utils
import BallTree.Search

type BallTree = BT.BallTree

ballTree :: Foldable m => Metric a -> SearchAlgorithm -> m a -> BT.BallTree a
ballTree = BT.buildBallTree

getNeighbors :: Ord a => Metric a -> a -> SearchAlgorithm -> BallTree a -> S.Set a
{-# INLINE getNeighbors #-}
{-# SPECIALIZE getNeighbors :: Metric (WithOffset a) -> WithOffset a -> SearchAlgorithm -> BallTree (WithOffset a) -> S.Set (WithOffset a) #-}
getNeighbors d p (BallSearch radius) = BT.ballSearch d p radius
getNeighbors d p (KnnSearch k) = undefined 
