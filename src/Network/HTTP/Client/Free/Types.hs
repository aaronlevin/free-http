{-| The primary Free Monad wrapping HTTP actions.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Network.HTTP.Client.Free.Types where

import Control.Monad.Trans.Free.Church (FT)
import Network.HTTP.Types.Method (StdMethod)

type family RequestType  client :: *
type family ResponseType client :: *

data HttpF client a = HttpF StdMethod (RequestType client) (ResponseType client -> a)
                    deriving Functor

type FreeHttp client m a = FT (HttpF client) m a
