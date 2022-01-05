{-# LANGUAGE OverloadedStrings #-}

module WebService.ScottyWS 
    ( runServer ) where

import Web.Scotty
import WebService.MapperAPI
import Control.Monad.IO.Class
import Data.Time

runServer :: IO ()
runServer = do
    putStrLn "Started server"
    scotty 8080 $ do
        post "/mapper/compute" $ do
            req <- jsonData :: ActionM MapperRequest
            t0 <- liftIO getCurrentTime 
            liftIO $ putStrLn $ "Received request: " ++ show t0
            let g = computeMapper req
            liftIO $ putStrLn $ "num of verts = " ++ show (Prelude.length $ vertices g)
            liftIO $ putStrLn $ "num of edges = " ++ show (Prelude.length $ edges g)
            t1 <- liftIO getCurrentTime 
            liftIO $ putStrLn $ "Computed response: " ++ show t1
            liftIO $ putStrLn $ "Elapsed time: " ++ show (diffUTCTime t1 t0)
            json g
    