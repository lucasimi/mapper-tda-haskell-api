module Utils where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Generic.Mutable as VGM

-- puts the element at index i in its correct position
-- returns the new index of the selected element
pivotST :: (VGM.MVector v a, Ord a) => v s a -> Int -> ST s Int
{-# INLINE pivotST #-}
pivotST vec i = do
    VGM.unsafeSwap vec 0 i
    p    <- VGM.unsafeRead vec 0
    hRef <- newSTRef 1
    VGM.iforM_ (VGM.unsafeDrop 1 vec) (\j xj -> do
        when (xj < p) $ do
            h <- readSTRef hRef
            VGM.unsafeSwap vec h (j + 1)
            modifySTRef' hRef (+1))
    h    <- readSTRef hRef
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
