{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                       (replicateM, foldM, foldM_)
import           Control.Monad.Trans.Free.Church     (FT)
import           Criterion.Main                      (bench, defaultMain, nfIO)
import           Data.ByteString.Lazy                (ByteString)
import           Network.HTTP.Client                 (Manager, Request,
                                                      Response,
                                                      defaultManagerSettings,
                                                      httpLbs, newManager,
                                                      parseUrl, responseBody)
import           Network.HTTP.Client.Free            (HttpF, get)
import           Network.HTTP.Client.Free.HttpClient (HttpClient, runHttp)
import System.Environment (getArgs)
import           System.Remote.Monitoring

type Client = FT (HttpF HttpClient) IO ByteString

freeProgram :: Int -> Request -> FT (HttpF HttpClient) IO [ByteString]
freeProgram i req = fmap responseBody <$> replicateM i (get req)

ioProgram :: Manager -> Int -> Request -> IO [ByteString]
ioProgram manager i req = fmap responseBody <$> replicateM i (httpLbs req manager)

foldProgram :: Int -> Request -> FT (HttpF HttpClient) IO ()
foldProgram i req = foldM_ (const . const $ get req) undefined [1..i]

foldIo :: Manager -> Int -> Request -> IO ()
foldIo manager i req = foldM_ (const . const $ httpLbs req manager) undefined [1..i]

main :: IO ()
main = do
    forkServer "localhost" 8000
    req <- parseUrl "http://localhost:8000"
    manager <- newManager defaultManagerSettings
    defaultMain [ bench "http-client: replicateM 1 request" $ nfIO (ioProgram manager 1 req)
                , bench "free-http: replicateM 1 request" $ nfIO (runHttp manager (freeProgram 1 req))
                , bench "http-client: replicateM 10 requests" $ nfIO (ioProgram manager 10 req)
                , bench "free-http: replicateM 10 requests" $ nfIO (runHttp manager (freeProgram 10 req))
                , bench "http-client: replicateM 100 requests" $ nfIO (ioProgram manager 100 req)
                , bench "free-http: replicateM 100 requests" $ nfIO (runHttp manager (freeProgram 100 req))
                , bench "http-client: replicateM 1000 requests" $ nfIO (ioProgram manager 1000 req)
                , bench "free-http: replicateM 1000 requests" $ nfIO (runHttp manager (freeProgram 1000 req))
                , bench "http-client: replicateM 10000 requests" $ nfIO (ioProgram manager 10000 req)
                , bench "free-http: replicateM 10000 requests" $ nfIO (runHttp manager (freeProgram 10000 req))
                , bench "http-client: foldM 1000 requests" $ nfIO (foldIo manager 1000 req)
                , bench "free-http: foldM 1000 requests" $ nfIO (runHttp manager (foldProgram 1000 req))
                ]

