module Mapper.Cover where

import Control.Monad
import Control.Monad.ST

import Data.Foldable

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.IntMap as M
import qualified Data.HashSet as S
import qualified Data.IntSet as IS

import Mapper.Domain
import Data.BallTree
import qualified Data.CircleTree.IntTreeNew as BT

type BallTree = BT.BallTree

type Offset = Int

type ClusterLabel = Int

data WithCover a = WithCover {-# UNPACK #-} !Offset [ClusterLabel]

updateLabel :: BallTree -> SearchAlgorithm -> VM.MVector s (WithCover a) -> Offset -> Int -> ST s Int
{-# INLINE updateLabel #-}
updateLabel bt sa vec xoff lbl = do
    WithCover _ xs <- VM.unsafeRead vec xoff
    if null xs
        then do
            let ids = BT.getNeighbors xoff sa bt
            forM_ ids $ \i -> do
                VM.unsafeModify vec (\(WithCover x ls) -> WithCover x (lbl:ls)) i
            return $ lbl + 1
        else return lbl

coverST :: BallTree -> SearchAlgorithm -> VM.MVector s (WithCover a) -> S.HashSet Offset -> ST s Graph
coverST bt sa vec s = do
    lbl <- foldrM (updateLabel bt sa vec) 0 s
    graph <- VM.generate lbl (const (Vertex IS.empty M.empty))
    populateGraphST graph vec

populateGraphST :: VM.MVector s Vertex -> VM.MVector s (WithCover a) -> ST s Graph
{-# INLINE populateGraphST #-}
populateGraphST graph vec = do
    VM.iforM_ vec $ \p (WithCover _ ls) -> do
        forM_ ls $ \l -> do
            VM.unsafeModify graph (\(Vertex ps es) -> Vertex (IS.insert p ps) es) l
    VM.forM_ vec $ \(WithCover _ ls) -> do
        forM_ ls $ \i -> do
            Vertex p0 e0 <- VM.unsafeRead graph i
            forM_ [j | j <- ls, j > i] $ \j -> do
                Vertex p1 e1 <- VM.unsafeRead graph j
                let e = edge p0 p1
                VM.unsafeWrite graph i (Vertex p0 (M.insert j e e0))
                VM.unsafeWrite graph j (Vertex p1 (M.insert i e e1))
    V.freeze graph

mapper :: (Foldable m) => m a -> Metric a -> SearchAlgorithm -> Graph
mapper vec d sa = runST $ do
    let v = V.fromList $ toList vec
        offDist i j = d (v V.! i) (v V.! j)
        v' = V.generate (V.length v) id
        vec' = V.generate (V.length v) (`WithCover` [])
        (bt, s) = BT.ballTree offDist sa v'
    u <- V.thaw vec'
    coverST bt sa u s
