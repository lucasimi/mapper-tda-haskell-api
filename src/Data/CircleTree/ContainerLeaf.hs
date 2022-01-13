module Data.CircleTree.ContainerLeaf
    ( BallTree
    , ballTree
    , getNeighbors ) where

import Control.Monad.ST
import Data.Foldable

import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.CircleTree.Common
import Data.Common
import Data.QuickSelect
import Data.BallTree

type LeafContainer = V.Vector

type CT a = CircleTree a (LeafContainer a)

data BallTree a = BallTree (Metric a) (CT a)

ballTree :: (Foldable m, Eq a, Hashable a) => Metric a -> SearchAlgorithm -> m a -> (BallTree a, S.HashSet a)
ballTree dist (BallSearch r) vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`WithDist` 0.0) (toList vec)
    (bt, s) <- ballTreeST dist r vec' S.empty 
    return (BallTree dist bt, s)
ballTree _ _ _ = undefined

getNeighbors :: (Eq a, Hashable a) => a -> SearchAlgorithm -> BallTree a -> S.HashSet a
getNeighbors p (BallSearch r) (BallTree d bt) = searchIter d p r bt $! S.empty
getNeighbors _ _ _ = undefined 

searchIter :: (Eq a, Hashable a) => Metric a -> a -> Float -> CT a -> S.HashSet a -> S.HashSet a
searchIter _ _ _ Empty s = s
searchIter dist p eps (Leaf v) s =
    let s' = S.fromList $ toList $ V.filter (\x -> dist p x <= eps) v
    in S.union s s' 
searchIter dist p eps Node { center = c, radius = r, left = x, right = y } s =
    let lSearch = if dist p c <= r + eps
            then searchIter dist p eps x $! s
            else s
        rSearch = if dist p c + eps > r
            then searchIter dist p eps y $! lSearch
            else lSearch
    in rSearch

ballTreeST :: (Eq a, Hashable a) => Metric a -> Float -> VM.MVector s (WithDist a) -> S.HashSet a -> ST s (CT a, S.HashSet a)
ballTreeST dist minRadius vec s =
    let n = VM.length vec
        m = n `div` 2
    in if n == 0
        then return (Empty, s)
    else if n == 1
        then do
            WithDist x _ <- VM.unsafeRead vec 0
            return (Leaf $ V.singleton x, S.insert x s)
        else do
            WithDist p _ <- VM.unsafeRead vec 0
            updateDistST vec dist
            _ <- quickSelectST vec m
            WithDist _ r <- VM.unsafeRead vec m
            let (vecL, vecR) = (VM.unsafeTake m vec, VM.unsafeDrop m vec)
            (lft, s') <- if r <= minRadius
                then do
                    vec' <- V.freeze vecL
                    return (Leaf $ V.map (\(WithDist x _) -> x) vec', S.insert p s)
                else ballTreeST dist minRadius vecL $! s
            (rgt, s'')    <- ballTreeST dist minRadius vecR $! s'
            return (Node { center = p
                          , radius = r
                          , left   = lft
                          , right  = rgt }, s'')
