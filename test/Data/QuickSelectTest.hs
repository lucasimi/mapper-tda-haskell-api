module Data.QuickSelectTest where

import Test.QuickCheck
import Test.QuickCheck.Instances.Vector

import Control.Monad.ST

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.QuickSelect

quickSelect :: Ord a => V.Vector a -> Int -> V.Vector a
quickSelect vec i = runST $ do
    vec' <- V.thaw vec
    quickSelectST vec' i
    V.freeze vec'

pivot :: Ord a => V.Vector a -> Int -> (Int, V.Vector a)
pivot vec i = runST $ do
    vec' <- V.thaw vec
    i' <- pivotST' vec' i
    vec'' <- V.freeze vec'
    return (i', vec'')

utilsTestSuite :: IO ()
utilsTestSuite = do
    quickCheck prop_PivotOrder
    quickCheck prop_QuickSelectOrder

prop_QuickSelectOrder :: V.Vector Float -> Int -> Bool 
prop_QuickSelectOrder vec i = 
    let n = V.length vec
        i' = i `mod` n
        vec' = quickSelect vec i'
        p = vec' V.! i'
    in (n == 0) 
       || and ([vec' V.! j <= p | j <- [0..i']] ++ [vec' V.! j >= p | j <- [i'..n-1]])

prop_PivotOrder :: V.Vector Float -> Int -> Bool 
prop_PivotOrder vec i = 
    let n = V.length vec
        i' = i `mod` n
        (i'', vec') = pivot vec i'
        p = vec' V.! i''
    in (n == 0) 
       || and ([vec' V.! j <= p | j <- [0..i'']] ++ [vec' V.! j >= p | j <- [i''..n-1]])
