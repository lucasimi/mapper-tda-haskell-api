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

type Label = Int

addLabelST :: Foldable m => STRef s Int -> VM.MVector s [Label] -> m Int -> ST s ()
addLabelST lblRef vec ids = do
    lbl <- readSTRef lblRef
    forM_ ids $ \i -> do
        lbls <- VM.read vec i
        VM.write vec i (lbl:lbls)
    writeSTRef lblRef (lbl + 1)

coverST :: Metric Int -> BallTree Int -> SearchAlgorithm -> VM.MVector s [Label] -> ST s Graph
coverST d bt sa vec = do
    lblRef <- newSTRef 0
    VM.iforM_ vec $ \i ls -> do
        when (null ls) $ do
            let ids = getNeighbors d i sa bt
            unless (null ids) $ do
                addLabelST lblRef vec ids
    lbl <- readSTRef lblRef
    graph <- VM.generate lbl (const (Vertex S.empty M.empty))
    populateGraphST graph vec

offsetMetric :: Metric a -> V.Vector a -> Metric Int
offsetMetric dist vec i j = dist (vec V.! i) (vec V.! j)

populateGraphST :: VM.MVector s Vertex -> VM.MVector s [Label] -> ST s Graph
populateGraphST graph vec = do
    VM.iforM_ vec $ \p ls -> do
        forM_ ls $ \l -> do
            Vertex ps es <- VM.read graph l
            VM.write graph l (Vertex (S.insert p ps) es)
    {--
    VM.iforM_ vec $ \p ls -> do
        forM_ [(i, j) | i <- ls, j <- ls, i /= j] $ \(i, j) -> do
            Vertex p0 e0 <- VM.read graph i
            Vertex p1 e1 <- VM.read graph j
            let e = edge p0 p1
            VM.write graph i (Vertex p0 (M.insert j e e0))
            VM.write graph j (Vertex p1 (M.insert i e e1))
    --}
    V.freeze graph

mapper :: Foldable m => m a -> Metric a -> SearchAlgorithm -> Graph
mapper vec d sa = runST $ do
    let n = length vec
        vec' = V.fromList $ toList vec
        d' = offsetMetric d vec'
        bt = ballTree d' [0..(n - 1)]
    u <- V.thaw $ V.replicate n []
    coverST d' bt sa u

type DataPoint = V.Vector Float
type Dataset   = V.Vector DataPoint

{--
toDataset :: (Foldable m1, Foldable m2) => m1 (m2 Float) -> Dataset
toDataset arr = V.fromList $ map (V.fromList . toList) (toList arr)
--}