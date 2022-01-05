{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module WebService.MapperAPI where

import Data.Aeson
import Data.Text
import Data.Char
import GHC.Generics

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

import Cover
import Domain ( Vertex (elements, relations), Metric, Graph, Edge (Edge), Dataset, DataPoint, Scalar, euclideanMetric, toDataset )
import BallTree.Search

buildTag :: String -> String
buildTag "" = ""
buildTag [c] = [Data.Char.toLower c]
buildTag (c:cs) = case Prelude.break isUpper cs of
    ([], _) -> [Data.Char.toLower c]
    (d, ds) -> Data.Char.toLower c:d

jsonOptions :: Options
jsonOptions = defaultOptions  
    { allNullaryToStringTag = False
    , sumEncoding           = TaggedObject "type" "" 
    , constructorTagModifier = buildTag
    }

newtype Point = Point { coordinates :: [Float] }
    deriving (Eq, Show, Read, Generic)
instance FromJSON Point
instance ToJSON Point

newtype DatasetRequest = DatasetRequest
    { points :: [Point] }
    deriving (Eq, Show, Read, Generic)
instance FromJSON DatasetRequest
instance ToJSON DatasetRequest

data LensRequest 
    = IdentityLensRequest
    | FeaturesLensRequest
    deriving (Eq, Show, Read, Generic)

instance FromJSON LensRequest where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON LensRequest where
    toJSON = genericToJSON jsonOptions

data MetricRequest
    = EuclideanMetricRequest
    | MinkowskiMetricRequest { p :: Float }
    | ChebyshevMetricRequest
    deriving (Eq, Show, Read, Generic)

instance FromJSON MetricRequest where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON MetricRequest where
    toJSON = genericToJSON jsonOptions

data CoverRequest 
    = TrivialCoverRequest
    | BallCoverRequest
        { metric :: MetricRequest 
        , lens   :: LensRequest
        , radius :: Float }
    | KnnCoverRequest
        { metric :: MetricRequest
        , lens   :: LensRequest
        , neighbors :: Int}
    deriving (Eq, Read, Show, Generic)

instance FromJSON CoverRequest where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON CoverRequest where
    toJSON = genericToJSON jsonOptions

data ClusteringRequest
    = TrivialClusteringRequest
    | DBSCANRequest 
        { metric'     :: MetricRequest 
        , eps        :: Float 
        , minSamples :: Int}
    deriving (Eq, Show, Read, Generic)

instance FromJSON ClusteringRequest where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ClusteringRequest where
    toJSON = genericToJSON jsonOptions

data MapperRequest = MapperRequest
    { dataset    :: DatasetRequest
    , cover      :: CoverRequest
    , clustering :: ClusteringRequest
    } deriving (Eq, Show, Read, Generic)
instance FromJSON MapperRequest
instance ToJSON MapperRequest

data GraphResponse = GraphResponse
    { vertices :: [VertexResponse]
    , edges    :: [EdgeResponse]
    } deriving (Eq, Show, Read, Generic)
instance FromJSON GraphResponse
instance ToJSON GraphResponse

data VertexResponse = VertexResponse
    { id  :: Int 
    , ids :: [Int] 
    } deriving (Eq, Show, Read, Generic)
instance FromJSON VertexResponse
instance ToJSON VertexResponse

data EdgeResponse = EdgeResponse
    { source    :: Int
    , adjacency :: [EdgeAdjacency] 
    } deriving (Eq, Show, Read, Generic)
instance FromJSON EdgeResponse
instance ToJSON EdgeResponse

data EdgeAdjacency = EdgeAdjacency
    { target     :: Int 
    , weight     :: Float 
    , similarity :: Float 
    } deriving (Eq, Show, Read, Generic)
instance FromJSON EdgeAdjacency
instance ToJSON EdgeAdjacency

toMetric :: MetricRequest -> Metric DataPoint 
toMetric EuclideanMetricRequest u v = euclideanMetric u v
toMetric _ _ _ = undefined

toGraph :: Graph -> GraphResponse
toGraph g = GraphResponse
    { vertices = [VertexResponse i (S.toList $ elements u) | (i, u) <- L.zip [0..] (V.toList g)] 
    , edges    = [EdgeResponse i 
                    [EdgeAdjacency j w s 
                    | (j, Edge s w) <- M.toList $ relations u] 
                 | (i, u) <- L.zip [0..] (V.toList g)] }

computeMapper :: MapperRequest -> GraphResponse
computeMapper MapperRequest 
    { dataset    = ds
    , cover      = BallCoverRequest
        { metric = EuclideanMetricRequest
        , lens   = IdentityLensRequest
        , radius = r }
    , clustering = TrivialClusteringRequest } = 
    let vec = toDataset $ L.map coordinates (points ds)
        d = toMetric EuclideanMetricRequest
        sa = BallSearch r
    in toGraph $ mapper vec d sa
computeMapper _ = undefined
