module Utils where

type Offset = Int

data WithOffset a = WithOffset a Offset

instance Eq (WithOffset a) where
    WithOffset _ i == WithOffset _ j = i == j

instance Ord (WithOffset a) where
    WithOffset _ i <= WithOffset _ j = i <= j

type ClusterLabel = Int

data WithCover a = WithCover a Offset [ClusterLabel]
