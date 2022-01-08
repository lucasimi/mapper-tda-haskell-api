{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module WebService.MapperAPI where

import Data.Aeson
import Data.Char
import GHC.Generics

import Data.Foldable

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import Mapper.Cover
import Mapper.Domain
import Data.BallTree
import Data.Time

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

newtype PointRequest = Point { coordinates :: [Float] }
    deriving (Eq, Show, Read, Generic)
instance FromJSON PointRequest
instance ToJSON PointRequest

newtype DatasetRequest = DatasetRequest
    { points :: [PointRequest] }
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

toMetric :: MetricRequest -> Metric Point 
toMetric EuclideanMetricRequest u v = euclideanMetric u v
toMetric _ _ _ = undefined

toGraph :: Graph -> GraphResponse
toGraph g = GraphResponse
    { vertices = [VertexResponse i (IS.toList $ elements u) | (i, u) <- zip [0..] (V.toList g)] 
    , edges    = [EdgeResponse i 
                    [EdgeAdjacency j w s 
                    | (j, Edge s w) <- IM.toList $ relations u] 
                 | (i, u) <- zip [0..] (V.toList g)] }

computeMapper :: MapperRequest -> GraphResponse
computeMapper MapperRequest 
    { dataset    = ds
    , cover      = BallCoverRequest
        { metric = EuclideanMetricRequest
        , lens   = IdentityLensRequest
        , radius = r }
    , clustering = TrivialClusteringRequest } = 
    let vec = toDataset $ map coordinates (points ds)
        d = toMetric EuclideanMetricRequest
        sa = BallSearch r
    in toGraph $ mapper vec d sa
computeMapper _ = undefined

processMapperRequest :: MapperRequest -> IO GraphResponse
processMapperRequest req = do
    t0 <- getCurrentTime 
    putStrLn $ "Received request: " ++ show t0
    let g = computeMapper req
    putStrLn $ "num of verts = " ++ show (Prelude.length $ vertices g)
    putStrLn $ "num of edges = " ++ show (Prelude.length $ edges g)
    t1 <- getCurrentTime 
    putStrLn $ "Computed response: " ++ show t1
    putStrLn $ "Elapsed time: " ++ show (diffUTCTime t1 t0)
    return g
