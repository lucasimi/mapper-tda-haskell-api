module BallTree.CircleTree.ContainerLeaf
    ( BallTree
    , buildBallTree
    , ballSearch ) where

import Control.Monad.ST
import Data.Foldable

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Domain
import Utils
import BallTree.Search
import BallTree.CircleTree.Common
import BallTree.Common

type LeafContainer = V.Vector
type BallTree a = CircleTree a (LeafContainer a)

buildBallTree :: Foldable m => Metric a -> SearchAlgorithm -> m a -> BallTree a
buildBallTree dist (BallSearch radius) vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`WithDist` 0.0) (toList vec)
    buildBallTreeST dist radius vec'
buildBallTree _ _ _ = undefined

ballSearch :: Ord a => Metric a -> a -> Float -> BallTree a -> S.Set a
{-# INLINE ballSearch #-}
{-# SPECIALIZE ballSearch :: Metric (WithOffset a) -> WithOffset a -> Float -> BallTree (WithOffset a) -> S.Set (WithOffset a) #-}
ballSearch _ _ _ Empty = S.empty
ballSearch dist p eps (Leaf x) =
    S.fromList $ toList $ V.filter (\x -> dist p x <= eps) x
ballSearch dist p eps Node { center = c, radius = r, left = x, right = y } =
    let lSearch = if dist p c <= r + eps
            then ballSearch dist p eps x
            else S.empty
        rSearch = if dist p c + eps > r
            then ballSearch dist p eps y
            else S.empty
    in S.union lSearch rSearch

buildBallTreeST :: Metric a -> Float -> VM.MVector s (WithDist a) -> ST s (BallTree a)
buildBallTreeST dist minRadius vec =
    let n = VM.length vec
        m = n `div` 2
    in if n == 0
        then return Empty
    else if n == 1
        then do
            WithDist x _ <- VM.read vec 0
            return $ Leaf $ V.singleton x
        else do
            WithDist p _ <- VM.read vec 0
            updateDistST vec dist
            quickSelectST vec m
            WithDist _ r <- VM.read vec m
            lft <- if r <= minRadius
                then do
                    vec' <- V.freeze $ VM.slice 0 m vec
                    return $ Leaf $ V.map (\(WithDist x d) -> x) vec'
                else buildBallTreeST dist minRadius (VM.slice 0 m vec)
            rgt          <- buildBallTreeST dist minRadius (VM.slice m (n - m) vec)
            return $ Node { center = p
                          , radius = r
                          , left   = lft
                          , right  = rgt }
