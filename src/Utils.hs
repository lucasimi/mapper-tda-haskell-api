module Utils where

import Data.Hashable

type Offset = Int

data WithOffset a = WithOffset a Offset

instance Eq (WithOffset a) where
    WithOffset _ i == WithOffset _ j = i == j

instance Ord (WithOffset a) where
    WithOffset _ i <= WithOffset _ j = i <= j

instance Hashable (WithOffset a) where
    hashWithSalt _ (WithOffset _ i) = i

type ClusterLabel = Int

data WithCover a = WithCover a Offset [ClusterLabel]
