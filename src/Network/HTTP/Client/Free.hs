{-| The primary Free Monad wrapping HTTP actions.
-}
{-# LANGUAGE TypeFamilies #-}

module Network.HTTP.Client.Free where

import Control.Monad.Trans.Free.Church (FT, liftF)
import Network.HTTP.Client (httpLbs, Manager, Request, Response)
import Network.HTTP.Client.Free.Types (HttpF(HttpF), RequestType, ResponseType)
import Network.HTTP.Types.Method (StdMethod(..))

-- | smart constructors
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
