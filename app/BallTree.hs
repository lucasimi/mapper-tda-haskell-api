module BallTree where

import Control.Monad
import Control.Monad.ST
import Data.STRef

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified GHC.Base as VM

newtype Metric a = Metric (a -> a -> Float)

data BallTree a
    = Leaf { points :: S.Set a }
    | Node { center :: a
           , radius :: Float
           , left   :: BallTree a
           , right  :: BallTree a
           }

dist :: a -> a -> Float
dist = undefined

buildBallTree :: V.Vector a -> BallTree a
buildBallTree vec = undefined

leq :: a -> a -> Bool
leq = undefined

-- for each element compute the distance
-- maybe use map and zip...
withDistFrom :: VM.MVector s a -> a -> Metric a -> ST s (VM.MVector s (a, Float))
withDistFrom vec point (Metric dist) = undefined

buildBallTreeST :: VM.MVector s a -> Metric a -> ST s (BallTree a)
buildBallTreeST vec (Metric dist) = undefined 

-- puts the element at index i in its correct position
-- returns the new index of the selected element
pivotST :: Ord a => VM.MVector s a -> Int -> ST s Int
pivotST vec i = do
    VM.swap vec 0 i
    p <- VM.read vec 0
    hRef <- newSTRef 1
    forM_ [1 .. (VM.length vec - 1)] $ \j -> do
        xj <- VM.read vec j
        when (xj <= p) $ do
            h <- readSTRef hRef
            VM.swap vec h j
            modifySTRef' hRef (+1)
    h <- readSTRef hRef
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

