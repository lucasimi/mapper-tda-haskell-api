module BallTree.CircleTree.ContainerLeaf
    ( BallTree
    , build
    , search ) where

import Control.Monad.ST
import Data.Foldable

import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Domain
import BallTree.Search
import BallTree.CircleTree.Common
import BallTree.Common
import Utils

type LeafContainer = V.Vector
type BallTree a = CircleTree a (LeafContainer a)

build :: Foldable m => Metric a -> SearchAlgorithm -> m a -> BallTree a
build dist (BallSearch radius) vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`WithDist` 0.0) (toList vec)
    buildST dist radius vec'
build _ _ _ = undefined

search :: (Eq a, Hashable a) => Metric a -> a -> Float -> BallTree a -> S.HashSet a
search = searchIter S.empty 

{--
search :: (Eq a, Hashable a) => Metric a -> a -> Float -> BallTree a -> S.HashSet a
search _ _ _ Empty = S.empty
search dist p eps (Leaf x) =
    S.fromList $ toList $ V.filter (\x -> dist p x <= eps) x
search dist p eps Node { center = c, radius = r, left = x, right = y } =
    let lSearch = if dist p c <= r + eps
            then search dist p eps x
            else S.empty
        rSearch = if dist p c + eps > r
            then search dist p eps y
            else S.empty
    in S.union lSearch rSearch
--}

searchIter :: (Eq a, Hashable a) => S.HashSet a -> Metric a -> a -> Float -> BallTree a -> S.HashSet a
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

buildST :: Metric a -> Float -> VM.MVector s (WithDist a) -> ST s (BallTree a)
buildST dist minRadius vec =
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
                    vec' <- V.freeze $ VM.unsafeSlice 0 m vec
                    return $ Leaf $ V.map (\(WithDist x d) -> x) vec'
                else buildST dist minRadius (VM.unsafeSlice 0 m vec)
            rgt          <- buildST dist minRadius (VM.unsafeSlice m (n - m) vec)
            return $ Node { center = p
                          , radius = r
                          , left   = lft
                          , right  = rgt }
