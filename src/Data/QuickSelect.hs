module Data.QuickSelect
    ( quickSelectST 
    , pivotST ) where

import Control.Monad.ST

import qualified Data.Vector.Generic.Mutable as VGM

pivotIterST :: (VGM.MVector v a, Ord a) => v s a -> a -> Int -> Int -> a -> ST s Int
{-# INLINE pivotIterST #-}
pivotIterST vec p h j xj = do
    if xj < p
        then do
            VGM.unsafeSwap vec h (j + 1)
            return $ h + 1
        else return h

pivotST :: (VGM.MVector v a, Ord a) => v s a -> Int -> ST s Int
{-# INLINE pivotST #-}
pivotST vec i = do
    VGM.unsafeSwap vec 0 i
    p <- VGM.unsafeRead vec 0
    h <- VGM.ifoldM' (pivotIterST vec p) 1 (VGM.unsafeDrop 1 vec)
    VGM.unsafeSwap vec 0 (h - 1)
    return $ h - 1

-- puts the ith statistics in its correct position
-- returns the previous index of the ith statistics
quickSelectST :: (VGM.MVector v a, Ord a) => v s a -> Int -> ST s Int
{-# INLINE quickSelectST #-}
quickSelectST vec i = do
    j <- pivotST vec i
    if i == j
        then return i
        else if i < j
            then quickSelectST (VGM.unsafeTake j vec) i
            else quickSelectST (VGM.unsafeDrop (j + 1) vec) (i - j - 1)
