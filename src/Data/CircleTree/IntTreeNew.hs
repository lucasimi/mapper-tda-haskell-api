module Data.CircleTree.IntTreeNew
    ( BallTree
    , ballTree
    , getNeighbors ) where

import Control.Monad.ST
import Data.Foldable

import qualified Data.HashSet as S
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Data.QuickSelect
import Data.BallTree

type Idx = Int

type IdxOrd = (Scalar, Idx)

updateDistST :: VUM.MVector s IdxOrd -> Metric Idx -> ST s ()
updateDistST vec dist = do
    (_, p) <- VUM.unsafeRead vec 0
    forM_ [0 .. VUM.length vec - 1] (VUM.unsafeModify vec (\(_, x) -> (dist p x, x)))

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
    vec' <- VU.thaw $ VU.fromList $ map (\x -> (0.0, x)) (toList vec)
    (bt, s) <- ballTreeST dist r vec' S.empty
    return (BT dist bt, s)
ballTree _ _ _ = undefined

getNeighbors :: Idx -> SearchAlgorithm -> BallTree -> S.HashSet Idx
getNeighbors p (BallSearch r) (BT d bt) = searchIter d p r bt $! S.empty
getNeighbors _ _ _ = undefined

searchIter :: Metric Idx -> Idx -> Scalar -> IntTree -> S.HashSet Idx -> S.HashSet Idx
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

ballTreeST :: Metric Idx -> Scalar -> VUM.MVector s IdxOrd -> S.HashSet Idx -> ST s (IntTree, S.HashSet Idx)
ballTreeST dist minRadius vec s =
    let n = VUM.length vec
        m = n `div` 2
    in if n == 0
        then return (Empty, s)
    else if n == 1
        then do
            (_, x) <- VUM.unsafeRead vec 0
            return (Leaf $ VU.singleton x, S.insert x s)
        else do
            (_, p) <- VUM.unsafeRead vec 0
            updateDistST vec dist
            _ <- quickSelectST vec m
            (r, _) <- VUM.unsafeRead vec m
            let (vecL, vecR) = (VUM.unsafeTake m vec, VUM.unsafeDrop m vec)
            (lft, s') <- if r <= minRadius
                then do
                    vec' <- VU.freeze vecL
                    return (Leaf $ VU.convert $ VU.map snd vec', S.insert p s)
                else ballTreeST dist minRadius vecL $! s
            (rgt, s'') <- ballTreeST dist minRadius vecR $! s'
            return (Node { center = p
                         , radius = r
                         , left   = lft
                         , right  = rgt }, s'')
