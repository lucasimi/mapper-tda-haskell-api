module Domain where

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Map as M

type Metric a = a -> a -> Float

type Scalar = Float

type DataPoint = V.Vector Scalar

type Dataset   = V.Vector DataPoint

data Vertex = Vertex
    { elements   :: S.Set Int
    , relations  :: M.Map Int Edge }

data Edge = Edge
    { similarity :: Float
    , weight     :: Float }

edge :: Ord a => S.Set a -> S.Set a -> Edge
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
