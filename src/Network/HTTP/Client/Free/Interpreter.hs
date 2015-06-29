{-| The primary Free Monad wrapping HTTP actions.
-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module Network.HTTP.Client.Free.Interpreter where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad.Trans.Free.Church (FT, liftF, iterT, iterTM)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (httpLbs, Manager, Request, Response)
import Network.HTTP.Client.Free.Types (HttpF(HttpF), ResponseType, RequestType)
import Network.HTTP.Types.Method (renderStdMethod)

data HttpClient
type instance RequestType  HttpClient = Request
type instance ResponseType HttpClient = Response ByteString

iterTHttp :: (Monad m, MonadIO m)
          => Manager
          -> HttpF HttpClient (m a)
          -> m a
iterTHttp manager (HttpF _ req next) = liftIO (httpLbs req manager) >>= next

iterTMHttp :: (Monad m, MonadTrans t, Monad (t m), MonadIO m)
    => Manager
    -> HttpF HttpClient (t m a)
    -> t m a
iterTMHttp manager (HttpF _ req next) = (lift . liftIO $ httpLbs req manager) >>= next

runTHttp :: (Monad m, MonadIO m)
         => Manager
         -> FT (HttpF HttpClient) m a
         -> m a
runTHttp manager = iterT (iterTHttp manager)

runTMHttp :: (Monad m, MonadIO m, MonadTrans t, Monad (t m))
        => Manager
        -> FT (HttpF HttpClient) m a
        -> t m a
runTMHttp manager = iterTM (iterTMHttp manager)
