module Data.Common where

import Control.Monad.ST

import qualified Data.Vector.Mutable as VM

import Data.BallTree

data WithDist a = WithDist a {-# UNPACK #-} !Scalar

instance Eq (WithDist a) where
    WithDist p0 d0 == WithDist p1 d1 = d0 == d1

instance Ord (WithDist a) where
    WithDist p0 d0 <= WithDist p1 d1 = d0 <= d1
    
updateDistST :: VM.MVector s (WithDist a) -> Metric a -> ST s ()
updateDistST vec dist = do
    (WithDist p _) <- VM.unsafeRead vec 0
    VM.iforM_ vec (\i (WithDist x _) -> do
        VM.unsafeWrite vec i (WithDist x (dist p x)))
