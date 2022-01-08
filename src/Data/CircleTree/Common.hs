module Data.CircleTree.Common where

import Data.BallTree

data CircleTree a b
    = Empty
    | Leaf b
    | Node { center :: a
           , radius :: {-# UNPACK #-} !Scalar
           , left   :: CircleTree a b
           , right  :: CircleTree a b
    } deriving Show
