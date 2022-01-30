module Data.BallTree.CircleTree
    ( BallTree
    , circleTree
    , getNeighbors ) where

import Data.Foldable

import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Data.BallTree

import qualified Data.BallTree.CircleTree.IntTree as IT

type Idx = Int 

data BallTree a = BallTree (Metric a) (V.Vector a) IT.IntTree

circleTree :: (Foldable m, Eq a, Hashable a) => Metric a -> SearchAlgorithm -> m a -> (BallTree a, S.HashSet a)
circleTree dist sa v =
    let vec = V.fromList $ toList v
        vec' = V.generate (V.length vec) id
        d i j = dist (V.unsafeIndex vec i) (V.unsafeIndex vec j)
        (IT.CircleTree _ bt, s) = IT.circleTree d sa vec'
    in (BallTree dist vec bt, S.map (V.unsafeIndex vec) s)

getNeighbors :: (Eq a, Hashable a) => a -> SearchAlgorithm -> BallTree a -> S.HashSet a
getNeighbors p (BallSearch r) (BallTree d v bt) = 
    let s = searchIter (\x i -> d x (V.unsafeIndex v i)) p r bt $! S.empty
    in S.map (V.unsafeIndex v) s
getNeighbors _ _ _ = undefined 

searchIter :: (a -> Idx -> Scalar) -> a -> Scalar -> IT.IntTree -> S.HashSet Idx -> S.HashSet Idx
searchIter _ _ _ IT.Empty s = s
searchIter dist p eps (IT.Leaf v) s =
    let s' = S.fromList $ VU.toList $ VU.filter (\x -> dist p x <= eps) v
    in S.union s s' 
searchIter dist p eps (IT.Node c r x y) s =
    let lSearch = if dist p c <= r + eps
            then searchIter dist p eps x $! s
            else s
        rSearch = if dist p c + eps > r
            then searchIter dist p eps y $! lSearch
            else lSearch
    in rSearch