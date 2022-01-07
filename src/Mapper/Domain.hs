module Mapper.Domain where

import Data.Foldable
import Data.Hashable

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import Data.CircleTree.Common
import Data.BallTree

type DataPoint = VU.Vector Scalar

type Dataset = V.Vector DataPoint

toDataPoint :: Foldable m => m Scalar -> DataPoint
toDataPoint = VU.fromList . toList

toDataset :: (Foldable m, Foldable n) => n (m Scalar) -> Dataset
toDataset v = V.fromList $ map toDataPoint (toList v)

euclideanMetric :: Metric DataPoint
euclideanMetric u v = sqrt $ VU.sum $ VU.zipWith (\x y -> let z = x - y in z * z) u v

data Vertex = Vertex
    { elements   :: IS.IntSet
    , relations  :: IM.IntMap Edge }

data Edge = Edge
    { similarity :: Float
    , weight     :: Float }

edge :: IS.IntSet -> IS.IntSet -> Edge
edge s1 s2 = Edge s w 
    where 
        s3 = IS.intersection s1 s2
        n1 = IS.size s1
        n2 = IS.size s2
        n3 = IS.size s3
        w = fromIntegral n3
        u = fromIntegral $ n1 + n2 - n3
        s = w / u

type Graph = V.Vector Vertex
