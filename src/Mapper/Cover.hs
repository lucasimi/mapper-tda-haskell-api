module Mapper.Cover where

import Control.Monad    
import Control.Monad.ST 

import Data.Foldable
import Data.Hashable

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.IntMap as M
import qualified Data.HashSet as S
import qualified Data.IntSet as IS

import Mapper.Domain
import Data.BallTree
import qualified Data.CircleTree.ContainerLeaf as BT

type BallTree a = BT.BallTree a 

type Offset = Int

data WithOffset a = WithOffset a {-# UNPACK #-} !Offset

instance Eq (WithOffset a) where
    WithOffset _ i == WithOffset _ j = i == j

instance Ord (WithOffset a) where
    WithOffset _ i <= WithOffset _ j = i <= j

instance Hashable (WithOffset a) where
    hashWithSalt _ (WithOffset _ i) = i

type ClusterLabel = Int

data WithCover a = WithCover {-# UNPACK #-} !Offset [ClusterLabel]

type OffsetPoint = WithOffset Point

updateLabel :: BallTree (WithOffset a) -> SearchAlgorithm -> VM.MVector s (WithCover a) -> WithOffset a -> Int -> ST s Int
{-# INLINE updateLabel #-}
updateLabel bt sa vec wo@(WithOffset _ xoff) lbl = do
    WithCover _ xs <- VM.unsafeRead vec xoff
    if null xs
        then do
            let ids = BT.getNeighbors wo sa bt
            forM_ ids $ \(WithOffset _ i) -> do
                VM.unsafeModify vec (\(WithCover x ls) -> WithCover x (lbl:ls)) i
            return $ lbl + 1
        else return lbl

coverST :: BallTree (WithOffset a) -> S.HashSet (WithOffset a) -> SearchAlgorithm -> VM.MVector s (WithCover a) -> ST s Graph
coverST bt s sa vec = do
    lbl <- foldrM (updateLabel bt sa vec) 0 s
    graph <- VM.generate lbl (const (Vertex IS.empty M.empty))
    populateGraphST graph vec

offsetMetric :: Metric a -> Metric (WithOffset a)
offsetMetric dist (WithOffset x _) (WithOffset y _) = dist x y

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
    let d' = offsetMetric d
        vec' = V.imap (flip WithOffset) (V.fromList $ toList vec)
        vec'' = V.map (\(WithOffset _ i) -> WithCover i []) vec'
        (bt, s) = BT.ballTree d' sa vec'
    u <- V.thaw vec''
    coverST bt s sa u
