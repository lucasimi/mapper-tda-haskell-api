module Data.CircleTreeTest where

import Test.QuickCheck
import Test.QuickCheck.Instances.Vector

import Control.Monad.ST (runST)

import Data.Foldable
import Data.Hashable

import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Mapper.Domain
import Data.CircleTreeNew
import Data.BallTree

circleTreeTestSuite :: IO ()
circleTreeTestSuite = do
    quickCheck $ prop_SearchSingleton absDist
    quickCheck $ prop_SearchCorrectness (absDist :: Metric Int)
    quickCheck $ prop_SearchContainsCenter (absDist :: Metric Int)

absDist :: Integral a => Metric a
absDist x y = fromIntegral $ abs $ x - y

naiveSearch :: (Foldable m, Eq a, Hashable a) => Metric a -> a -> Float -> m a -> S.HashSet a
naiveSearch dist p eps = foldr (\x s ->
    if dist x p <= eps then S.insert x s else s) S.empty

prop_SearchSingleton :: Metric Int -> V.Vector Int -> Int -> Bool 
prop_SearchSingleton dist points i = 
    let (bt, _) = circleTree dist (BallSearch 0.1) points
        i' = i `mod` V.length points
        c = points V.! i'
    in (V.length points == 0) || getNeighbors c (BallSearch 0.1) bt == S.singleton c

prop_SearchCorrectness :: (Eq a, Hashable a) => Metric a -> V.Vector a -> Int -> Float -> Bool
prop_SearchCorrectness dist points i r =
    let (bt, _) = circleTree dist (BallSearch r) points
        i' = i `mod` V.length points
        c = points V.! i'
    in getNeighbors c (BallSearch r) bt == naiveSearch dist c r points

prop_SearchContainsCenter :: (Eq a, Hashable a) => Metric a -> V.Vector a -> Int -> Float -> Bool
prop_SearchContainsCenter dist points idx eps =
    let (bt, _) = circleTree dist (BallSearch eps) points
        i = idx `mod` V.length points
        p = points V.! i
    in (V.length points == 0) || (eps <= 0) || S.member p (getNeighbors p (BallSearch eps) bt)
