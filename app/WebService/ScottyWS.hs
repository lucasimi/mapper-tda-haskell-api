{-# LANGUAGE OverloadedStrings #-}

module WebService.ScottyWS 
    ( runServer ) where

import Web.Scotty
import WebService.MapperAPI
import Control.Monad.IO.Class

runServer :: IO ()
runServer = do
    putStrLn "Started server"
    scotty 8080 $ do
        post "/mapper/compute" $ do
            req <- jsonData :: ActionM MapperRequest
            g <- liftIO $ processMapperRequest req
            json g
