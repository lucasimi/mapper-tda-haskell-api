module Cover where

import Control.Monad    ( forM_, when, unless )
import Control.Monad.ST ( runST, ST )
import Data.STRef       ( modifySTRef', newSTRef, readSTRef, writeSTRef, STRef )
import Data.Foldable    ( Foldable(toList) )

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Map as M

import Domain
import BallTree
import Utils
import BallTree.Search

addClusterLabelST :: Foldable m => STRef s Int -> VM.MVector s (WithCover a) -> m (WithOffset a) -> ST s ()
addClusterLabelST lblRef vec ids = do
    lbl <- readSTRef lblRef
    forM_ ids $ \(WithOffset _ i) -> do
        WithCover x xoff lbls <- VM.read vec i
        VM.write vec i (WithCover x xoff (lbl:lbls))
    writeSTRef lblRef (lbl + 1)

coverST :: Metric (WithOffset a) -> BallTree (WithOffset a) -> SearchAlgorithm -> VM.MVector s (WithCover a) -> ST s Graph
coverST d bt sa vec = do
    lblRef <- newSTRef 0
    VM.iforM_ vec $ \i (WithCover x xoff ls) -> do
        when (null ls) $ do
            let ids = getNeighbors d (WithOffset x xoff) sa bt
            unless (null ids) $ do
                addClusterLabelST lblRef vec ids
    lbl <- readSTRef lblRef
    graph <- VM.generate lbl (const (Vertex S.empty M.empty))
    populateGraphST graph vec

offsetMetric :: Metric a -> Metric (WithOffset a)
offsetMetric dist (WithOffset x _) (WithOffset y _) = dist x y

populateGraphST :: VM.MVector s Vertex -> VM.MVector s (WithCover a) -> ST s Graph
populateGraphST graph vec = do
    VM.iforM_ vec $ \p (WithCover _ _ ls) -> do
        forM_ ls $ \l -> do
            Vertex ps es <- VM.read graph l
            VM.write graph l (Vertex (S.insert p ps) es)
    VM.iforM_ vec $ \p (WithCover _ _ ls) -> do
        forM_ [(i, j) | i <- ls, j <- ls, i /= j] $ \(i, j) -> do
            Vertex p0 e0 <- VM.read graph i
            Vertex p1 e1 <- VM.read graph j
            let e = edge p0 p1
            VM.write graph i (Vertex p0 (M.insert j e e0))
            VM.write graph j (Vertex p1 (M.insert i e e1))
    V.freeze graph

mapper :: Foldable m => m a -> Metric a -> SearchAlgorithm -> Graph
mapper vec d sa = runST $ do
    let n = length vec
        d' = offsetMetric d
        vec' = V.imap (flip WithOffset) (V.fromList $ toList vec)
        vec'' = V.map (\(WithOffset x i) -> WithCover x i []) vec'
        bt = ballTree d' sa vec'
    u <- V.thaw vec''
    coverST d' bt sa u

