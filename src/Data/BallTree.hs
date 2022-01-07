module Data.BallTree where

type Scalar = Float

type Metric a = a -> a -> Scalar

data SearchAlgorithm
    = BallSearch Float 
    | KnnSearch Int 
