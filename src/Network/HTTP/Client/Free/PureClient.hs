{-| A pure interepreter
-}

{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Network.HTTP.Client.Free.PureClient (
      runHttp
    , runTHttp
) where

import           Control.Monad.Trans             (MonadTrans)
import           Control.Monad.Trans.Free.Church (FT, iterT, iterTM, liftF)
import           Network.HTTP.Client             (Manager, Request, Response,
                                                  httpLbs)
import           Network.HTTP.Client.Free.Types  (HttpF (HttpF), RequestType,
                                                  ResponseType)

-------------------------------------------------------------------------------
-- | Peel a layer of the 'HttpF' functor and, given the pure, mock function
-- that maps requests to responses, run the next method against the returned
-- request.
iterTHttp :: (Monad m)
          => (RequestType client -> ResponseType client)
          -- ^ a function to mock requests
          -> HttpF client (m a)
          -> m a
iterTHttp mock (HttpF _ req next) = next (mock req)

-------------------------------------------------------------------------------
-- | Peel a layer of the 'HttpF' functor and, given the pure, mock function
-- that maps requests to responses, run the next method against the returned
-- request. the base monad for this action is `t m a`
iterTMHttp :: (Monad m, MonadTrans t, Monad (t m))
           => (RequestType client -> ResponseType client)
           -> HttpF client (t m a)
           -> t m a
iterTMHttp mock (HttpF _ req next) = next (mock req)

-------------------------------------------------------------------------------
-- | A pure interpreter based on a client-supplied mocking function
runHttp :: Monad m
        => (RequestType client -> ResponseType client)
        -> ignore
        -- ^ a parameter that will be ignored. It is included so client's can
        -- hot-swap interpreters (many will require a `Manager` type)
        -> FT (HttpF client) m a
        -> m a
runHttp mock _ = iterT (iterTHttp mock)

-------------------------------------------------------------------------------
-- | A pure interpreter based on a client-supplied mocking function. The under-
-- lying monad is `t m`, so computations will be lifted into `t m`.
runTHttp :: (Monad m, MonadTrans t, Monad (t m))
         => (RequestType client -> ResponseType client)
         -> ignore
         -- ^ a paramter that will be ignored. It is included so client's can
         -- host-swap interpreters (many will require a 'Manager' type)
         -> FT (HttpF client) m a
         -> t m a
runTHttp mock _ = iterTM (iterTMHttp mock)
