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
import Data.STRef

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

addClusterLabelST :: Foldable m => STRef s Int -> VM.MVector s (WithCover a) -> m (WithOffset a) -> ST s ()
addClusterLabelST lblRef vec ids = do
    lbl <- readSTRef lblRef
    forM_ ids $ \(WithOffset _ i) -> do
        VM.unsafeModify vec (\(WithCover x ls) -> WithCover x (lbl:ls)) i
        --WithCover xoff lbls <- VM.unsafeRead vec i
        --VM.unsafeWrite vec i (WithCover xoff (lbl:lbls))
    writeSTRef lblRef (lbl + 1)

coverST :: BallTree (WithOffset a) -> S.HashSet (WithOffset a) -> SearchAlgorithm -> VM.MVector s (WithCover a) -> ST s Graph
coverST bt s sa vec = do
    lblRef <- newSTRef 0
    forM_ s $ \(WithOffset x xoff) -> do
        WithCover _ ls <- VM.unsafeRead vec xoff
        when (null ls) $ do
            let ids = BT.getNeighbors (WithOffset x xoff) sa bt
            unless (null ids) $ do
                addClusterLabelST lblRef vec ids
    lbl <- readSTRef lblRef
    graph <- VM.generate lbl (const (Vertex IS.empty M.empty))
    populateGraphST graph vec

{--
coverST' bt s sa vec = do
    forM_ (zip (toList s) [0..]) $  \(wo@(WithOffset _ xi), l) -> do
        WithCover _ xs <- VM.unsafeRead vec xi
        when (null xs) $ do
            let ids = BT.getNeighbors wo sa bt
            unless (null ids) $ do
                forM_ ids $ \(WithOffset _ xoff) -> do
                    VM.unsafeModify vec (\(WithCover i ls) -> WithCover i (l:ls)) xoff
--}



coverPoint :: Int -> BallTree (WithOffset a) -> WithOffset a -> SearchAlgorithm -> V.Vector (WithCover a) -> V.Vector (WithCover a)
coverPoint lbl bt wo sa v =
    let ids = BT.getNeighbors wo sa bt
        ups = [(i, 0) | WithOffset _ i <- S.toList ids]
    in V.accum (\(WithCover i lbls) _ -> WithCover i (lbl:lbls)) v ups 


buildCoverIter :: Int -> BallTree (WithOffset a) -> [WithOffset a] -> SearchAlgorithm -> V.Vector (WithCover a) -> V.Vector (WithCover a)
buildCoverIter _ _ [] _ v = v
buildCoverIter l bt (c@(WithOffset _ xoff):cs) sa v =
    let v' = buildCoverIter l bt cs sa v
        WithCover _ ls = v' V.! xoff 
    in if null ls
        then coverPoint (l + 1) bt c sa v'
        else v'



buildCover :: BallTree (WithOffset a) -> [WithOffset a] -> SearchAlgorithm -> V.Vector (WithCover a) -> V.Vector (WithCover a)
buildCover = buildCoverIter 0




offsetMetric :: Metric a -> Metric (WithOffset a)
offsetMetric dist (WithOffset x _) (WithOffset y _) = dist x y

populateGraphST :: VM.MVector s Vertex -> VM.MVector s (WithCover a) -> ST s Graph
populateGraphST graph vec = do
    VM.iforM_ vec $ \p (WithCover _ ls) -> do
        forM_ ls $ \l -> do
            Vertex ps es <- VM.unsafeRead graph l
            VM.unsafeWrite graph l (Vertex (IS.insert p ps) es)
    VM.forM_ vec $ \(WithCover _ ls) -> do
        forM_ [(i, j) | i <- ls, j <- ls, i /= j] $ \(i, j) -> do
            Vertex p0 e0 <- VM.unsafeRead graph i
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
