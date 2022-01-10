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

type CT a = CircleTree a a

data BallTree a = BallTree (Metric a) (CT a)

ballTree :: Foldable m => Metric a -> SearchAlgorithm -> m a -> BallTree a
ballTree dist _ vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`WithDist` 0.0) (toList vec)
    bt <- ballTreeST dist vec'
    return $ BallTree dist bt

getNeighbors :: (Eq a, Hashable a) => a -> SearchAlgorithm -> BallTree a -> S.HashSet a
getNeighbors p (BallSearch r) (BallTree d bt) = searchIter S.empty d p r bt
getNeighbors _ _ _ = undefined 

searchIter :: (Eq a, Hashable a) => S.HashSet a -> Metric a -> a -> Float -> CT a -> S.HashSet a
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

ballTreeST :: Metric a -> VM.MVector s (WithDist a) -> ST s (CT a)
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
            _ <- quickSelectST vec m
            WithDist _ r <- VM.unsafeRead vec m
            let (vecL, vecR) = (VM.unsafeTake m vec, VM.unsafeDrop m vec)
            lft          <- ballTreeST dist vecL
            rgt          <- ballTreeST dist vecR
            return $ Node { center = p
                          , radius = r
                          , left   = lft
                          , right  = rgt }
