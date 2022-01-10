module Data.Common where

import Control.Monad.ST

import qualified Data.Vector.Mutable as VM

import Data.BallTree
import Data.Foldable

data WithDist a = WithDist a {-# UNPACK #-} !Scalar

instance Eq (WithDist a) where
    WithDist _ d0 == WithDist _ d1 = d0 == d1

instance Ord (WithDist a) where
    WithDist _ d0 <= WithDist _ d1 = d0 <= d1

updateDistST :: VM.MVector s (WithDist a) -> Metric a -> ST s ()
updateDistST vec dist = do
    (WithDist p _) <- VM.unsafeRead vec 0
    for_ [0 .. VM.length vec - 1] (VM.unsafeModify vec (\(WithDist x _) -> WithDist x (dist p x)))