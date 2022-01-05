module Domain where

import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as M
import qualified Data.Foldable as F
import Data.Hashable

type Scalar = Float

type Metric a = a -> a -> Scalar

type DataPoint = VU.Vector Scalar

type Dataset   = V.Vector DataPoint

toDataPoint :: Foldable m => m Scalar -> DataPoint
toDataPoint = VU.fromList . F.toList

toDataset :: (Foldable m, Foldable n) => n (m Scalar) -> Dataset
toDataset v = V.fromList $ map toDataPoint (F.toList v)

euclideanMetric :: Metric DataPoint
euclideanMetric u v = sqrt $ VU.sum $ VU.zipWith (\x y -> let z = x - y in z * z) u v

data Vertex = Vertex
    { elements   :: S.HashSet Int
    , relations  :: M.HashMap Int Edge }

data Edge = Edge
    { similarity :: Float
    , weight     :: Float }

edge :: (Eq a, Hashable a) => S.HashSet a -> S.HashSet a -> Edge
edge s1 s2 = Edge s w 
    where 
        s3 = S.intersection s1 s2
        n1 = S.size s1
        n2 = S.size s2
        n3 = S.size s3
        w = fromIntegral n3
        u = fromIntegral $ n1 + n2 - n3
        s = w / u

type Graph = V.Vector Vertex
