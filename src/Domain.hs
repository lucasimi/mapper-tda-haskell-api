module Domain where

import qualified Data.Set as S
import qualified Data.Vector as V

type Metric a = a -> a -> Float

data Vertex = Vertex
    { points     :: S.Set Int
    , adjaciency :: [Int] }

data Edge = Edge
    { target :: Int 
    , weight :: Int }

type Graph = V.Vector Vertex
