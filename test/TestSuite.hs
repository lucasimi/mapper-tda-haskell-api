module Main where

import Test.QuickCheck

import qualified Data.Set as S
import Data.Traversable
import Data.Foldable

import BallTree

main :: IO ()
main = do
    quickCheck (searchProp (Metric absDist) :: [Int] -> Int -> Float -> Bool)

absDist :: Integral a => a -> a -> Float
absDist x y = fromIntegral $ abs $ x - y

naiveSearch :: (Traversable m, Ord a) => m a -> Metric a -> a -> Float -> S.Set a
naiveSearch points (Metric dist) p eps = foldr (\x s -> if dist x p <= eps then S.insert x s else s) S.empty points

searchProp :: Ord a => Metric a -> [a] -> a -> Float -> Bool
searchProp dist points c r = 
    let mt = buildMetricTree points dist
    in S.map (\(WithDist x _) -> x) (ballSearch mt c r) == naiveSearch points dist c r
