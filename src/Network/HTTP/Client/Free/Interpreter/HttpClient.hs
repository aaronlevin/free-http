{-| An Interpreter with http-client as the foundation
-}
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module Network.HTTP.Client.Free.Interpreter.HttpClient (
    HttpClient
    , runHttp
    , runTHttp
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad.Trans.Free.Church (FT, liftF, iterT, iterTM)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (httpLbs, Manager, Request, Response)
import Network.HTTP.Client.Free.Types (HttpF(HttpF), ResponseType, RequestType)
import Network.HTTP.Client.Free.Util (setMethod)
import Network.HTTP.Types.Method (renderStdMethod)

-------------------------------------------------------------------------------
-- | 'HttpClient' is an uninhabited type used to identify the http-client
-- based interpreter
data HttpClient

-------------------------------------------------------------------------------
-- | HttpClient expects 'Request's and returns 'Response ByteString's
type instance RequestType  HttpClient = Request
type instance ResponseType HttpClient = Response ByteString

-------------------------------------------------------------------------------
-- | Peel a layer of the 'HttpF' functor and run an http request with the data
-- provided.
iterTHttp :: (Monad m, MonadIO m)
          => Manager
          -> HttpF HttpClient (m a)
          -> m a
iterTHttp manager (HttpF m r next) =
    let !req = setMethod m r in liftIO (httpLbs req manager) >>= next

-------------------------------------------------------------------------------
-- | Peel a layer of the 'HttpF' functor and run an http request with the data.
-- the base monad for this action is 't m'.
iterTMHttp :: (Monad m, MonadTrans t, Monad (t m), MonadIO m)
    => Manager
    -> HttpF HttpClient (t m a)
    -> t m a
iterTMHttp manager (HttpF m r next) =
    let !req = setMethod m r in (lift . liftIO $ httpLbs req manager) >>= next

-------------------------------------------------------------------------------
-- | The main http-client interpreter. The client is free to specify the base
-- effect monad so long as there is an instance of 'MonadIO' for it in scope.
runHttp :: (Monad m, MonadIO m)
         => Manager
         -> FT (HttpF HttpClient) m a
         -> m a
runHttp manager = iterT (iterTHttp manager)

-------------------------------------------------------------------------------
-- | The main http-client interpreter. The client is free to specify the base
-- effect monad ('m'), and in thise case this the result can be lifted into a
-- higher monad transformer stack ('t')
runTHttp :: (Monad m, MonadIO m, MonadTrans t, Monad (t m))
        => Manager
        -> FT (HttpF HttpClient) m a
        -> t m a
runTHttp manager = iterTM (iterTMHttp manager)
