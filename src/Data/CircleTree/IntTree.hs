module Data.CircleTree.IntTree
    ( BallTree
    , ballTree
    , getNeighbors ) where

import Control.Monad.ST
import Data.Foldable

import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM

import Data.QuickSelect
import Data.BallTree

type Offset = Int

data WithDist = WithDist {-# UNPACK #-} !Offset {-# UNPACK #-} !Scalar

instance Eq WithDist where
    WithDist _ d0 == WithDist _ d1 = d0 == d1

instance Ord WithDist where
    WithDist _ d0 <= WithDist _ d1 = d0 <= d1

updateDistST :: VM.MVector s WithDist -> Metric Offset -> ST s ()
updateDistST vec dist = do
    WithDist p _ <- VM.unsafeRead vec 0
    forM_ [0 .. VM.length vec - 1] (VM.unsafeModify vec (\(WithDist x _) -> WithDist x (dist p x)))

data CT
    = Empty
    | Leaf (VU.Vector Offset)
    | Node { center :: {-# UNPACK #-} !Offset
           , radius :: {-# UNPACK #-} !Scalar
           , left   :: CT
           , right  :: CT
    } deriving Show

data BallTree a = BallTree (Metric Offset) CT

ballTree :: (Foldable m, Eq a, Hashable a) => Metric Offset -> SearchAlgorithm -> m Offset -> (BallTree a, S.HashSet Offset)
ballTree dist (BallSearch r) vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`WithDist` 0.0) (toList vec)
    (bt, s) <- ballTreeST dist r vec' S.empty 
    return (BallTree dist bt, s)
ballTree _ _ _ = undefined

getNeighbors :: (Eq a, Hashable a) => Offset -> SearchAlgorithm -> BallTree a -> S.HashSet Offset
getNeighbors p (BallSearch r) (BallTree d bt) = searchIter d p r bt $! S.empty
getNeighbors _ _ _ = undefined 

searchIter :: Metric Offset -> Offset -> Float -> CT -> S.HashSet Offset -> S.HashSet Offset
searchIter _ _ _ Empty s = s
searchIter dist p eps (Leaf v) s =
    let s' = S.fromList $ VU.toList $ VU.filter (\x -> dist p x <= eps) v
    in S.union s s' 
searchIter dist p eps Node { center = c, radius = r, left = x, right = y } s =
    let lSearch = if dist p c <= r + eps
            then searchIter dist p eps x $! s
            else s
        rSearch = if dist p c + eps > r
            then searchIter dist p eps y $! lSearch
            else lSearch
    in rSearch

ballTreeST :: Metric Offset -> Float -> VM.MVector s WithDist -> S.HashSet Offset -> ST s (CT, S.HashSet Offset)
ballTreeST dist minRadius vec s =
    let n = VM.length vec
        m = n `div` 2
    in if n == 0
        then return (Empty, s)
    else if n == 1
        then do
            WithDist x _ <- VM.unsafeRead vec 0
            return (Leaf $ VU.singleton x, S.insert x s)
        else do
            WithDist p _ <- VM.unsafeRead vec 0
            updateDistST vec dist
            _ <- quickSelectST vec m
            WithDist _ r <- VM.unsafeRead vec m
            let (vecL, vecR) = (VM.unsafeTake m vec, VM.unsafeDrop m vec)
            (lft, s') <- if r <= minRadius
                then do
                    vec' <- V.freeze vecL
                    return (Leaf $ VU.convert $ V.map (\(WithDist x _) -> x) vec', S.insert p s)
                else ballTreeST dist minRadius vecL $! s
            (rgt, s'') <- ballTreeST dist minRadius vecR $! s'
            return (Node { center = p
                         , radius = r
                         , left   = lft
                         , right  = rgt }, s'')
