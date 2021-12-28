module Cover where

import Control.Monad    ( forM_, when )
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

coverST :: Metric Int -> BallTree Int -> Float -> VM.MVector s [Label] -> ST s Graph
coverST d bt radius vec = do
    lblRef <- newSTRef 0
    VM.iforM_ vec $ \i ls -> do
        when (null ls) $ do
            let ids = S.map (\(WithDist x _) -> x) (ballSearch d i radius bt)
            addLabelST lblRef vec ids
    lbl <- readSTRef lblRef
    graph <- VM.generate lbl (const (Vertex S.empty []))
    populateGraphST graph vec

offsetMetric :: Metric a -> V.Vector a -> Metric Int 
offsetMetric dist vec i j = dist (vec V.! i) (vec V.! j)

populateGraphST :: VM.MVector s Vertex -> VM.MVector s [Label] -> ST s Graph
populateGraphST graph vec = do
    VM.iforM_ vec $ \p ls -> do
        forM_ ls $ \l -> do
            Vertex ps es <- VM.read graph l
            VM.write graph l (Vertex (S.insert p ps) es)
    VM.iforM_ vec $ \p ls -> do
        forM_ [(i, j) | i <- ls, j <- ls, i /= j] $ \(i, j) -> do
            Vertex p0 l0 <- VM.read graph i
            Vertex p1 l1 <- VM.read graph j
            VM.write graph i (Vertex p0 (j:l0)) 
            VM.write graph i (Vertex p0 (i:l1))
    V.freeze graph

cover :: Foldable m => m a -> Metric a -> Float -> Graph
cover vec d r = runST $ do
    let n = length vec
        d' = offsetMetric d (V.fromList $ toList vec)
        bt = buildBallTree d' [0..(n - 1)]
    vec' <- V.thaw $ V.fromList $ toList vec
    v <- V.thaw $ V.replicate n []
    coverST d' bt r v
