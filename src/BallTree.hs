module BallTree where

import qualified Data.Set as S
import qualified Data.Vector as V

import Domain
import qualified BallTree.Internal as BT

type BallTree = BT.BallTree

ballTree :: Foldable m => Metric a -> m a -> BT.BallTree a
ballTree = BT.buildBallTree

data SearchAlgorithm
    = BallSearch Float 
    | KnnSearch Int 

getNeighbors :: Ord a => Metric a -> a -> SearchAlgorithm -> BallTree a -> S.Set a
getNeighbors d p (BallSearch radius) = BT.ballSearch d p radius
getNeighbors d p (KnnSearch k) = undefined 
