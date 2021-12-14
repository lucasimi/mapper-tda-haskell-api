module BallTree where

import Control.Monad    ( forM_, when )
import Control.Monad.ST ( runST, ST )
import Data.STRef       ( modifySTRef', newSTRef, readSTRef )
import Data.Foldable    ( Foldable(toList) )

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified GHC.Base as VM

newtype Metric a = Metric (a -> a -> Float)

data BallTree a
    = Empty
    | Leaf { point  :: a }
    | Node { center :: a
           , radius :: Float
           , left   :: BallTree a
           , right  :: BallTree a }

data MetricTree a = MetricTree 
    { metric :: Metric a
    , tree   :: BallTree a }

data WithDist a = WithDist a Float

instance Eq (WithDist a) where
    WithDist _ x == WithDist _ y = x == y

instance Ord (WithDist a) where
    WithDist _ x <= WithDist _ y = x <= y

updateDistST :: VM.MVector s (WithDist a) -> Metric a -> ST s ()
updateDistST vec (Metric dist) = do
    (WithDist p _) <- VM.read vec 0
    VM.iforM_ vec (\i (WithDist x _) -> do
        VM.write vec i (WithDist x (dist p x)))

buildBallTree :: Traversable m => m a -> Metric a -> BallTree a
buildBallTree vec dist = runST $ do
    vec' <- V.thaw $ V.fromList $ toList $ fmap (`WithDist` 0.0) vec
    buildBallTreeST vec' dist

ballSearch :: MetricTree a -> a -> Float -> S.Set (WithDist a)
ballSearch (MetricTree _ Empty) _ _ = S.empty 
ballSearch (MetricTree (Metric dist) (Leaf x)) p eps = 
    let d = dist p x
    in if d <= eps 
        then S.singleton (WithDist x d) 
        else S.empty 
ballSearch (MetricTree (Metric dist) Node { center = c, radius = r, left = x, right = y }) p eps = 
    let lSearch = if dist p c <= r + eps 
            then ballSearch (MetricTree (Metric dist) x) p eps
            else S.empty
        rSearch = if dist p c + eps > r
            then ballSearch (MetricTree (Metric dist) y) p eps
            else S.empty
    in S.union lSearch rSearch

buildMetricTree :: Traversable m => m a -> Metric a -> MetricTree a
buildMetricTree vec dist = MetricTree 
    { metric = dist 
    , tree   = buildBallTree vec dist }

buildBallTreeST :: VM.MVector s (WithDist a) -> Metric a -> ST s (BallTree a)
buildBallTreeST vec dist =
    let n = VM.length vec
        m = n `div` 2
    in if n == 0
        then return Empty
    else if n == 1
        then do
            WithDist x _ <- VM.read vec 0
            return $ Leaf { point = x }
        else do
            p            <- VM.read vec 0
            updateDistST vec dist
            quickSelectST vec m
            WithDist c r <- VM.read vec m
            lft          <- buildBallTreeST (VM.slice 0 m vec) dist
            rgt          <- buildBallTreeST (VM.slice m (n - m) vec) dist
            return $ Node { center = c
                          , radius = r
                          , left   = lft
                          , right  = rgt }

-- puts the element at index i in its correct position
-- returns the new index of the selected element
pivotST :: Ord a => VM.MVector s a -> Int -> ST s Int
{-# INLINE pivotST #-}
pivotST vec i = do
    VM.swap vec 0 i
    p    <- VM.read vec 0
    hRef <- newSTRef 1
    VM.iforM_ (VM.slice 1 (VM.length vec - 1) vec) (\j xj -> do
        when (xj <= p) $ do
            h <- readSTRef hRef
            VM.swap vec h j
            modifySTRef' hRef (+1))
    h    <- readSTRef hRef
    VM.swap vec 0 (h - 1)
    return $ h - 1

-- puts the ith statistics in its correct position
-- returns the previous index of the ith statistics
quickSelectST :: Ord a => VM.MVector s a -> Int -> ST s Int
quickSelectST vec i = do
    j <- pivotST vec i
    if i == j
        then return i
        else if i < j
            then quickSelectST (VM.slice 0 j vec) i
            else quickSelectST (VM.slice j (VM.length  vec - j) vec) (i - j)

-- extras

updateDistST' :: VM.MVector s (WithDist a) -> Metric a -> ST s ()
updateDistST' vec (Metric dist) = do
    (WithDist p _) <- VM.read vec 0
    forM_ [0..VM.length vec - 1] (\i -> do          -- TODO: compare performace with VM.iforM_ version
        WithDist x _ <- VM.read vec i
        VM.write vec i (WithDist x (dist p x)))

pivotST' :: Ord a => VM.MVector s a -> Int -> ST s Int
pivotST' vec i = do
    VM.swap vec 0 i
    p    <- VM.read vec 0
    hRef <- newSTRef 1
    forM_ [1 .. (VM.length vec - 1)] $ \j -> do     -- TODO: compare performance with VM.iforM_ version
        xj <- VM.read vec j
        when (xj <= p) $ do
            h <- readSTRef hRef
            VM.swap vec h j
            modifySTRef' hRef (+1)
    h    <- readSTRef hRef
    VM.swap vec 0 (h - 1)
    return $ h - 1
