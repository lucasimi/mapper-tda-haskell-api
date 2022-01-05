{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module WebService.ServantWS 
    ( runServer ) where

import Servant.API
import Network.Wai.Handler.Warp
import Servant
import WebService.MapperAPI
import Network.Wai.Logger
import Control.Monad.IO.Class
import Data.Time

type API = "mapper" :> "compute" :> ReqBody '[JSON] MapperRequest :> Post '[JSON] GraphResponse

mapperAPI :: Proxy API
mapperAPI = Proxy

server :: Server API
server = res
  where 
    res :: MapperRequest -> Handler GraphResponse
    res req = do
        t0 <- liftIO getCurrentTime 
        liftIO $ putStrLn $ "Received request: " ++ show t0
        let g = computeMapper req
        liftIO $ putStrLn $ "num of verts = " ++ show (Prelude.length $ vertices g)
        liftIO $ putStrLn $ "num of edges = " ++ show (Prelude.length $ edges g)
        t1 <- liftIO getCurrentTime 
        liftIO $ putStrLn $ "Computed response: " ++ show t1
        liftIO $ putStrLn $ "Elapsed time: " ++ show (diffUTCTime t1 t0)
        return g

runServer :: IO ()
runServer = 
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings (serve mapperAPI server)