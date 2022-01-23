module Data.CircleTree.IntTreeLazy
    ( BallTree
    , ballTree
    , getNeighbors
    , mergeSort ) where

import Control.Parallel
import Data.Foldable

import qualified Data.HashSet as S
import qualified Data.Vector.Unboxed as VU

import Data.BallTree

type Idx = Int

type IdxOrd = (Scalar, Idx)

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
ballTree dist (BallSearch r) vec =
    let vec' = map (\x -> (0.0, x)) (toList vec)
        (bt, s) = ballTree' dist r vec' S.empty 
    in (BT dist bt, s)
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

ballTree' :: Metric Idx -> Scalar -> [IdxOrd] -> S.HashSet Idx -> (IntTree, S.HashSet Idx)
ballTree' dist minRadius = ballTreeIter
    where
        ballTreeIter [] s = (Empty, s)
        ballTreeIter [(_, x)] s = (Leaf $ VU.singleton x, S.insert x s)
        ballTreeIter v@((_, p):_) s =
            let n = length v
                m = n `div` 2
                v' = map (\(_, x) -> (dist p x, x)) v
                v'' = mergeSort v'
                (r, _) = v'' !! m
                (vecL, vecR) = splitAt m v''
                (lft, s') = if r <= minRadius
                    then (Leaf $ VU.fromList $ map snd v'', S.insert p s)
                    else ballTreeIter vecL s
                (rgt, s'') = ballTreeIter vecR s'
            in (lft, s') `par` ((rgt, s'') `pseq` (
                Node { center = p
                     , radius = r
                     , left   = lft
                     , right  = rgt }, s''))

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let (ys, zs) = split xs
    in merge (mergeSort ys) (mergeSort zs)
    where
        split [] = ([], [])
        split [x] = ([x], [])
        split (x:y:ys) =
            let (zs, ws) = split ys
            in (x:zs, y:ws)
        merge [] ys' = ys'
        merge xs' [] = xs'
        merge xx@(x':xs') yy@(y':ys') = if x' <= y'
            then x':merge xs' yy
            else y':merge xx ys'
