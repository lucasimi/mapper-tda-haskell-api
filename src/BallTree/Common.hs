module BallTree.Common where

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Data.STRef

import Domain

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

-- puts the element at index i in its correct position
-- returns the new index of the selected element
pivotST :: Ord a => VM.MVector s a -> Int -> ST s Int
{-# INLINE pivotST #-}
{-# SPECIALIZE pivotST :: VM.MVector s (WithDist a) -> Int -> ST s Int #-}
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
{-# INLINE quickSelectST #-}
{-# SPECIALIZE quickSelectST :: VM.MVector s (WithDist a) -> Int -> ST s Int #-}
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
