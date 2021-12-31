module BallTree.InternalTestSuite where

import Test.QuickCheck
import Test.QuickCheck.Instances.Vector

import Control.Monad.ST (runST)

import qualified Data.Set as S
import Data.Foldable
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Domain
import BallTree.Internal

internalTestSuite :: IO ()
internalTestSuite = do
    quickCheck prop_PivotOrder
    quickCheck prop_QuickSelectOrder
    quickCheck $ prop_SearchSingleton absDist
    quickCheck $ prop_SearchCorrectness (absDist :: Metric Int)
    quickCheck $ prop_SearchContainsCenter (absDist :: Metric Int)

absDist :: Integral a => Metric a
absDist x y = fromIntegral $ abs $ x - y

naiveSearch :: (Foldable m, Ord a) => Metric a -> a -> Float -> m a -> S.Set a
naiveSearch dist p eps = foldr (\x s ->
    if dist x p <= eps then S.insert x s else s) S.empty

prop_SearchSingleton :: Metric Int -> V.Vector Int -> Int -> Bool 
prop_SearchSingleton dist points i = 
    let bt = buildBallTree dist points
        i' = i `mod` V.length points
        c = points V.! i'
    in (V.length points == 0) || ballSearch dist c 0.1 bt == S.singleton c

prop_SearchCorrectness :: Ord a => Metric a -> V.Vector a -> Int -> Float -> Bool
prop_SearchCorrectness dist points i r =
    let bt = buildBallTree dist points
        i' = i `mod` V.length points
        c = points V.! i'
    in ballSearch dist c r bt == naiveSearch dist c r points

prop_SearchContainsCenter :: Ord a => Metric a -> V.Vector a -> Int -> Float -> Bool
prop_SearchContainsCenter dist points idx eps =
    let bt = buildBallTree dist points
        i = idx `mod` V.length points
        p = points V.! i
    in (V.length points == 0) || (eps <= 0) || S.member p (ballSearch dist p eps bt)


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

