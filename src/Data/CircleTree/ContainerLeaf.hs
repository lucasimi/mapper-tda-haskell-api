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

data BallTree a = BallTree
    { metric :: Metric a
    , tree   :: CT a }

ballTree :: Foldable m => Metric a -> SearchAlgorithm -> m a -> BallTree a
ballTree dist (BallSearch radius) vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`WithDist` 0.0) (toList vec)
    bt <- ballTreeST dist radius vec'
    return $ BallTree dist bt
ballTree _ _ _ = undefined

getNeighbors :: (Eq a, Hashable a) => a -> SearchAlgorithm -> BallTree a -> S.HashSet a
getNeighbors p (BallSearch r) (BallTree d bt) = searchIter S.empty d p r bt
getNeighbors _ _ _ = undefined 

searchIter :: (Eq a, Hashable a) => S.HashSet a -> Metric a -> a -> Float -> CT a -> S.HashSet a
searchIter s _ _ _ Empty = s
searchIter s dist p eps (Leaf x) =
    let s' = S.fromList $ toList $ V.filter (\x -> dist p x <= eps) x
    in S.union s s' 
searchIter s dist p eps Node { center = c, radius = r, left = x, right = y } =
    let lSearch = if dist p c <= r + eps
            then searchIter s dist p eps x
            else s
        rSearch = if dist p c + eps > r
            then searchIter lSearch dist p eps y
            else lSearch
    in rSearch

ballTreeST :: Metric a -> Float -> VM.MVector s (WithDist a) -> ST s (CT a)
ballTreeST dist minRadius vec =
    let n = VM.length vec
        m = n `div` 2
    in if n == 0
        then return Empty
    else if n == 1
        then do
            WithDist x _ <- VM.unsafeRead vec 0
            return $ Leaf $ V.singleton x
        else do
            WithDist p _ <- VM.unsafeRead vec 0
            updateDistST vec dist
            quickSelectST vec m
            WithDist _ r <- VM.unsafeRead vec m
            lft <- if r <= minRadius
                then do
                    vec' <- V.freeze $ VM.unsafeTake m vec
                    return $ Leaf $ V.map (\(WithDist x d) -> x) vec'
                else ballTreeST dist minRadius (VM.unsafeTake m vec)
            rgt          <- ballTreeST dist minRadius (VM.unsafeDrop m vec)
            return $ Node { center = p
                          , radius = r
                          , left   = lft
                          , right  = rgt }
