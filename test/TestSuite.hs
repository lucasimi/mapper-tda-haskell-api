module Main where

import Test.QuickCheck

import qualified Data.Set as S
import Data.Foldable

import BallTree

main :: IO ()
main = do
    quickCheck (prop_SearchCorrectness (absDist :: Metric Int))
    quickCheck (prop_SearchContainsCenter (absDist :: Metric Int))

absDist :: Integral a => Metric a
absDist = Metric (\x y -> fromIntegral $ abs $ x - y)

naiveSearch :: Foldable m => m a -> Metric a -> a -> Float -> S.Set (WithDist a)
naiveSearch points (Metric dist) p eps = foldr (\x s -> 
    let d = dist x p
    in if d <= eps then S.insert (WithDist x d) s else s) S.empty points

prop_SearchCorrectness :: Metric a -> [a] -> a -> Float -> Bool
prop_SearchCorrectness dist points c r = 
    let mt = buildMetricTree points dist
    in ballSearch mt c r == naiveSearch points dist c r

prop_SearchContainsCenter :: Metric a -> [a] -> Float -> Bool
prop_SearchContainsCenter dist points eps = 
    let mt = buildMetricTree points dist
    in and [ballSearch mt x eps == naiveSearch points dist x eps | x <- points]
