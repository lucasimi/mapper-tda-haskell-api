module Data.CircleTree.SingletonLeaf
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
import Data.BallTree
import Data.Common
import Data.QuickSelect

type BallTree a = CircleTree a a

ballTree :: Foldable m => Metric a -> SearchAlgorithm -> m a -> BallTree a
ballTree dist (BallSearch radius) vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`WithDist` 0.0) (toList vec)
    ballTreeST dist vec'
ballTree _ _ _ = undefined

getNeighbors :: (Eq a, Hashable a) => Metric a -> a -> SearchAlgorithm -> BallTree a -> S.HashSet a
getNeighbors d p (BallSearch r) bt = searchIter S.empty d p r bt
getNeighbors _ _ _ _ = undefined 

searchIter :: (Eq a, Hashable a) => S.HashSet a -> Metric a -> a -> Float -> BallTree a -> S.HashSet a
searchIter s _ _ _ Empty = s
searchIter s dist p eps (Leaf x) = 
    if dist p x <= eps then S.insert x s else s
searchIter s dist p eps Node { center = c, radius = r, left = x, right = y } =
    let lSearch = if dist p c <= r + eps
            then searchIter s dist p eps x
            else s
        rSearch = if dist p c + eps > r
            then searchIter lSearch dist p eps y
            else lSearch
    in rSearch

ballTreeST :: Metric a -> VM.MVector s (WithDist a) -> ST s (BallTree a)
ballTreeST dist vec =
    let n = VM.length vec
        m = n `div` 2
    in if n == 0
        then return Empty
    else if n == 1
        then do
            WithDist x _ <- VM.unsafeRead vec 0
            return $ Leaf x
        else do
            WithDist p _ <- VM.unsafeRead vec 0
            updateDistST vec dist
            quickSelectST vec m
            WithDist _ r <- VM.unsafeRead vec m
            lft          <- ballTreeST dist (VM.unsafeTake m vec)
            rgt          <- ballTreeST dist (VM.unsafeDrop m vec)
            return $ Node { center = p
                          , radius = r
                          , left   = lft
                          , right  = rgt }