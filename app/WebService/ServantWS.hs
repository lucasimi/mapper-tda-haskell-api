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

type API = "mapper" :> "compute" :> ReqBody '[JSON] MapperRequest :> Post '[JSON] GraphResponse

mapperAPI :: Proxy API
mapperAPI = Proxy

server :: Server API
server = res
  where 
    res :: MapperRequest -> Handler GraphResponse
    res req = liftIO $ processMapperRequest req

runServer :: IO ()
runServer = 
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings (serve mapperAPI server)
