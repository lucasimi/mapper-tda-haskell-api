module Mapper.Cover where

import Control.Monad    
import Control.Monad.ST 

import Data.STRef       
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
import Data.CircleTree.Common

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

data WithCover a = WithCover a {-# UNPACK #-} !Offset [ClusterLabel]

type OffsetPoint = WithOffset DataPoint

addClusterLabelST :: Foldable m => STRef s Int -> VM.MVector s (WithCover a) -> m (WithOffset a) -> ST s ()
addClusterLabelST lblRef vec ids = do
    lbl <- readSTRef lblRef
    forM_ ids $ \(WithOffset _ i) -> do
        WithCover x xoff lbls <- VM.unsafeRead vec i
        VM.unsafeWrite vec i (WithCover x xoff (lbl:lbls))
    writeSTRef lblRef (lbl + 1)

coverST :: Metric (WithOffset a) -> BallTree (WithOffset a) -> SearchAlgorithm -> VM.MVector s (WithCover a) -> ST s Graph
coverST d bt sa vec = do
    lblRef <- newSTRef 0
    VM.iforM_ vec $ \i (WithCover x xoff ls) -> do
        when (null ls) $ do
            let ids = BT.getNeighbors (WithOffset x xoff) sa bt
            unless (null ids) $ do
                addClusterLabelST lblRef vec ids
    lbl <- readSTRef lblRef
    graph <- VM.generate lbl (const (Vertex IS.empty M.empty))
    populateGraphST graph vec

offsetMetric :: Metric a -> Metric (WithOffset a)
offsetMetric dist (WithOffset x _) (WithOffset y _) = dist x y

populateGraphST :: VM.MVector s Vertex -> VM.MVector s (WithCover a) -> ST s Graph
populateGraphST graph vec = do
    VM.iforM_ vec $ \p (WithCover _ _ ls) -> do
        forM_ ls $ \l -> do
            Vertex ps es <- VM.unsafeRead graph l
            VM.unsafeWrite graph l (Vertex (IS.insert p ps) es)
    VM.iforM_ vec $ \p (WithCover _ _ ls) -> do
        forM_ [(i, j) | i <- ls, j <- ls, i /= j] $ \(i, j) -> do
            Vertex p0 e0 <- VM.unsafeRead graph i
            Vertex p1 e1 <- VM.unsafeRead graph j
            let e = edge p0 p1
            VM.unsafeWrite graph i (Vertex p0 (M.insert j e e0))
            VM.unsafeWrite graph j (Vertex p1 (M.insert i e e1))
    V.freeze graph

mapper :: Foldable m => m a -> Metric a -> SearchAlgorithm -> Graph
mapper vec d sa = runST $ do
    let n = length vec
        d' = offsetMetric d
        vec' = V.imap (flip WithOffset) (V.fromList $ toList vec)
        vec'' = V.map (\(WithOffset x i) -> WithCover x i []) vec'
        bt = BT.ballTree d' sa vec'
    u <- V.thaw vec''
    coverST d' bt sa u
