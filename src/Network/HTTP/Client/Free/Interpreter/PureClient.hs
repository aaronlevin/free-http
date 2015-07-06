{-| A pure interepreter
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Network.HTTP.Client.Free.Interpreter.PureClient (
    PureClient
    -- , runHttp
    -- , runTHttp
) where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (httpLbs, Manager, Request, Response)
import Network.HTTP.Client.Free.Types (ResponseType, RequestType)

-------------------------------------------------------------------------------
-- | 'PureClient' is an uninhabited type used to identify the pure interpreter
data PureClient

-------------------------------------------------------------------------------
-- | PureClient expects 'Request's and returns 'Response ByteString's
type instance RequestType PureClient = Request
type instance ResponseType PureClient = Response ByteString
