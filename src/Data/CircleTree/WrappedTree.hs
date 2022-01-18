module Data.CircleTree.WrappedTree where

import Data.Foldable
import Data.Hashable

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashSet as S

import Data.BallTree
import qualified Data.CircleTree.IntTree as IT

data BallTree a = WBT (Metric a) (V.Vector a) IT.CT

offsetMetric :: V.Vector a -> Metric a -> Metric Int
offsetMetric vec d i j = d (vec V.! i) (vec V.! j)

offsetVector :: V.Vector a -> V.Vector Int
offsetVector vec = V.generate (V.length vec) id

ballTree :: (Foldable m, Eq a, Hashable a) => Metric a -> SearchAlgorithm -> m a -> (BallTree a, S.HashSet a)
ballTree dist sa vec =
    let v = V.fromList $ toList vec
        (IT.BT _ ct, s) = IT.ballTree (offsetMetric v dist) sa (offsetVector v)
        s' = S.map (v V.!) s
    in (WBT dist v ct, s')

getNeighbors :: (Eq a, Hashable a) => a -> SearchAlgorithm -> BallTree a -> S.HashSet a
getNeighbors p (BallSearch r) (WBT d vec bt) = searchIter vec d p r bt $! S.empty
getNeighbors _ _ _ = undefined

searchIter :: (Eq a, Hashable a) => V.Vector a -> Metric a -> a -> Float -> IT.CT -> S.HashSet a -> S.HashSet a
searchIter _ _ _ _ IT.Empty s = s
searchIter vec dist p eps (IT.Leaf v) s =
    let s' = S.fromList $ VU.toList $ VU.filter (\x -> dist p (vec V.! x) <= eps) v
        s'' = S.map (vec V.!) s'
    in S.union s s''
searchIter vec dist p eps IT.Node { IT.center = c, IT.radius = r, IT.left = x, IT.right = y } s =
    let lSearch = if dist p (vec V.! c) <= r + eps
            then searchIter vec dist p eps x $! s
            else s
        rSearch = if dist p (vec V.! c) + eps > r
            then searchIter vec dist p eps y $! lSearch
            else lSearch
    in rSearch
