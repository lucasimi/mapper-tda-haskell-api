module BallTree.Internal where

import Control.Monad    ( forM_, when )
import Control.Monad.ST ( runST, ST )
import Data.STRef       ( modifySTRef', newSTRef, readSTRef, writeSTRef, STRef )
import Data.Foldable    ( Foldable(toList) )

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Map as M

import Domain

data BallTree a
    = Empty
    | Leaf a
    | Node { center :: a
           , radius :: Float
           , left   :: BallTree a
           , right  :: BallTree a 
    } deriving Show

data WithDist a = WithDist 
    { point    :: a
    , distance :: Float }

instance Eq (WithDist a) where
    WithDist p0 d0 == WithDist p1 d1 = d0 == d1

instance Ord (WithDist a) where
    WithDist p0 d0 <= WithDist p1 d1 = d0 <= d1

updateDistST :: VM.MVector s (WithDist a) -> Metric a -> ST s ()
updateDistST vec dist = do
    (WithDist p _) <- VM.read vec 0
    VM.iforM_ vec (\i (WithDist x _) -> do
        VM.write vec i (WithDist x (dist p x)))

buildBallTree :: Foldable m => Metric a -> m a -> BallTree a
buildBallTree dist vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`WithDist` 0.0) (toList vec)
    buildBallTreeST dist vec'

ballSearch :: Ord a => Metric a -> a -> Float -> BallTree a -> S.Set a
ballSearch _ _ _ Empty = S.empty
ballSearch dist p eps (Leaf x) =
    if dist p x <= eps
        then S.singleton x
        else S.empty
ballSearch dist p eps Node { center = c, radius = r, left = x, right = y } =
    let lSearch = if dist p c <= r + eps
            then ballSearch dist p eps x
            else S.empty
        rSearch = if dist p c + eps > r
            then ballSearch dist p eps y
            else S.empty
    in S.union lSearch rSearch

buildBallTreeST :: Metric a -> VM.MVector s (WithDist a) -> ST s (BallTree a)
buildBallTreeST dist vec =
    let n = VM.length vec
        m = n `div` 2
    in if n == 0
        then return Empty
    else if n == 1
        then do
            WithDist x _ <- VM.read vec 0
            return $ Leaf x
        else do
            WithDist p _ <- VM.read vec 0
            updateDistST vec dist
            quickSelectST vec m
            WithDist _ r <- VM.read vec m
            lft          <- buildBallTreeST dist (VM.slice 0 m vec)
            rgt          <- buildBallTreeST dist (VM.slice m (n - m) vec)
            return $ Node { center = p
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
        when (xj < p) $ do
            h <- readSTRef hRef
            VM.swap vec h (j + 1)
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
            else quickSelectST (VM.slice (j + 1) (VM.length vec - j - 1) vec) (i - j - 1)

quickSelect :: Ord a => V.Vector a -> Int -> V.Vector a
quickSelect vec i = runST $ do
    vec' <- V.thaw vec
    quickSelectST vec' i
    V.freeze vec'

pivot :: Ord a => V.Vector a -> Int -> (Int, V.Vector a)
pivot vec i = runST $ do
    vec' <- V.thaw vec
    i' <- pivotST vec' i
    vec'' <- V.freeze vec'
    return (i', vec'')

-- extras

updateDistST' :: Metric a -> VM.MVector s (WithDist a) -> ST s ()
updateDistST' dist vec = do
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
