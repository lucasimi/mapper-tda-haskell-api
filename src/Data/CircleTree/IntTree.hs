module Data.CircleTree.IntTree
    ( BallTree
    , ballTree
    , getNeighbors ) where

import Control.Monad.ST
import Data.Foldable

import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM

import Data.QuickSelect
import Data.BallTree

type Idx = Int

data IdxOrd = IdxOrd {-# UNPACK #-} !Idx {-# UNPACK #-} !Scalar

instance Eq IdxOrd where
    IdxOrd _ d0 == IdxOrd _ d1 = d0 == d1

instance Ord IdxOrd where
    IdxOrd _ d0 <= IdxOrd _ d1 = d0 <= d1

updateDistST :: VM.MVector s IdxOrd -> Metric Idx -> ST s ()
updateDistST vec dist = do
    IdxOrd p _ <- VM.unsafeRead vec 0
    forM_ [0 .. VM.length vec - 1] (VM.unsafeModify vec (\(IdxOrd x _) -> IdxOrd x (dist p x)))

data IntTree
    = Empty
    | Leaf (VU.Vector Idx)
    | Node { center :: {-# UNPACK #-} !Idx
           , radius :: {-# UNPACK #-} !Scalar
           , left   :: IntTree
           , right  :: IntTree
    } deriving Show

data BallTree = BT (Metric Idx) IntTree

ballTree :: Foldable m => Metric Idx -> SearchAlgorithm -> m Idx -> (BallTree, S.HashSet Idx)
ballTree dist (BallSearch r) vec = runST $ do
    vec' <- V.thaw $ V.fromList $ map (`IdxOrd` 0.0) (toList vec)
    (bt, s) <- ballTreeST dist r vec' S.empty 
    return (BT dist bt, s)
ballTree _ _ _ = undefined

getNeighbors :: Idx -> SearchAlgorithm -> BallTree -> S.HashSet Idx
getNeighbors p (BallSearch r) (BT d bt) = search d p r bt $! S.empty
getNeighbors _ _ _ = undefined 

search :: Metric Idx -> Idx -> Scalar -> IntTree -> S.HashSet Idx -> S.HashSet Idx
search dist p eps = searchIter
    where 
        searchIter Empty s = s
        searchIter (Leaf v) s =
            let s' = S.fromList $ VU.toList $ VU.filter (\x -> dist p x <= eps) v
            in S.union s s' 
        searchIter Node { center = c, radius = r, left = x, right = y } s =
            let lSearch = if dist p c <= r + eps
                    then searchIter x $! s
                    else s
                rSearch = if dist p c + eps > r
                    then searchIter y $! lSearch
                    else lSearch
            in rSearch

ballTreeST :: Metric Idx -> Scalar -> VM.MVector s IdxOrd -> S.HashSet Idx -> ST s (IntTree, S.HashSet Idx)
ballTreeST dist minRadius = ballTreeIterST
    where 
        ballTreeIterST vec s =
            let n = VM.length vec
                m = n `div` 2
            in if n == 0
                then return (Empty, s)
            else if n == 1
                then do
                    IdxOrd x _ <- VM.unsafeRead vec 0
                    return (Leaf $ VU.singleton x, S.insert x s)
                else do
                    IdxOrd p _ <- VM.unsafeRead vec 0
                    updateDistST vec dist
                    _ <- quickSelectST vec m
                    IdxOrd _ r <- VM.unsafeRead vec m
                    let (vecL, vecR) = (VM.unsafeTake m vec, VM.unsafeDrop m vec)
                    (lft, s') <- if r <= minRadius
                        then do
                            vec' <- V.freeze vecL
                            return (Leaf $ VU.convert $ V.map (\(IdxOrd x _) -> x) vec', S.insert p s)
                        else ballTreeIterST vecL $! s
                    (rgt, s'') <- ballTreeIterST vecR $! s'
                    return (Node { center = p
                                , radius = r
                                , left   = lft
                                , right  = rgt }, s'')
