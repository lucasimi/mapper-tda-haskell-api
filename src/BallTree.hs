module BallTree where

import Data.Hashable
import qualified Data.HashSet as S

import Domain
import BallTree.Search
import qualified BallTree.CircleTree.ContainerLeaf as BT

type BallTree a = BT.BallTree a

ballTree :: Foldable m => Metric a -> SearchAlgorithm -> m a -> BT.BallTree a
ballTree = BT.build

getNeighbors :: (Eq a, Hashable a) => Metric a -> a -> SearchAlgorithm -> BallTree a -> S.HashSet a
{-# INLINE getNeighbors #-}
getNeighbors d p (BallSearch radius) = BT.search d p radius
getNeighbors d p (KnnSearch k) = undefined 
