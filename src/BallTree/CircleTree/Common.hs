module BallTree.CircleTree.Common where

data CircleTree a b
    = Empty
    | Leaf b
    | Node { center :: a
           , radius :: Float
           , left   :: CircleTree a b
           , right  :: CircleTree a b
    } deriving Show
