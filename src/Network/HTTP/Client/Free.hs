{-| The primary Free Monad wrapping HTTP actions.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Network.HTTP.Client.Free (

    -- * Type Families
    -- ** Base Request type
      RequestType
    -- ** Base Response type
    , ResponseType

    -- * Types
    -- ** The base functor from which our free monad is generated.
    , HttpF(HttpF)
    -- ** A helpful type alias
    , FreeHttp

    -- * Handy morphisms for working with HttpF
    , natHttpF
    , transHttp

    -- * smart constructors for http verbs
    , connect
    , delete
    , get
    , head
    , options
    , patch
    , post
    , put
    , trace

) where

import Control.Monad.Trans.Free.Church (FT, liftF, transFT)
import Network.HTTP.Client (httpLbs, Manager, Request, Response)
import Network.HTTP.Client.Free.Types (FreeHttp, HttpF(HttpF), RequestType, ResponseType)
import Network.HTTP.Types.Method (StdMethod(..))
import Prelude hiding (head)

get :: Monad m
    => RequestType client
    -> FT (HttpF client) m (ResponseType client)
get req = liftF (HttpF GET req id)

post :: Monad m
     => RequestType client
     -> FT (HttpF client) m (ResponseType client)
post req = liftF (HttpF POST req id)

head :: Monad m
     => RequestType client
     -> FT (HttpF client) m (ResponseType client)
head req = liftF (HttpF HEAD req id)

put :: Monad m
    => RequestType client
    -> FT (HttpF client) m (ResponseType client)
put req = liftF (HttpF PUT req id)

delete :: Monad m
       => RequestType client
       -> FT (HttpF client) m (ResponseType client)
delete req = liftF (HttpF DELETE req id)

trace :: Monad m
      => RequestType client
      -> FT (HttpF client) m (ResponseType client)
trace req = liftF (HttpF TRACE req id)

connect :: Monad m
        => RequestType client
        -> FT (HttpF client) m (ResponseType client)
connect req = liftF (HttpF CONNECT req id)

options :: Monad m
        => RequestType client
        -> FT (HttpF client) m (ResponseType client)
options req = liftF (HttpF OPTIONS req id)

patch :: Monad m
      => RequestType client
      -> FT (HttpF client) m (ResponseType client)
patch req = liftF (HttpF PATCH req id)

-- | A natural transformation between 'HttpF' types.
natHttpF :: (RequestType client1 -> RequestType client2)
        -> (ResponseType client2 -> ResponseType client1)
        -> HttpF client1 a
        -> HttpF client2 a
natHttpF reqT respT (HttpF method req resp) = HttpF method (reqT req) (resp . respT)

-- | 'transHttp' allows clients to mix-and-match http request and response
-- foundations, so long as there is an appropriate morphism.
transHttp :: Monad m
          => (RequestType client1 -> RequestType client2)
          -> (ResponseType client2 -> ResponseType client1)
          -> FreeHttp client1 m a
          -> FreeHttp client2 m a
transHttp reqT respT = transFT (natHttpF reqT respT)
