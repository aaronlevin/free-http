{-| Utilities for working with networking types
-}

{-# LANGUAGE BangPatterns #-}

module Network.HTTP.Client.Free.Util (
    setMethod
) where

import Network.HTTP.Client (Request(method))
import Network.HTTP.Types.Method (StdMethod, renderStdMethod)

-- | set the method of a request, overriding the previous method.
setMethod :: StdMethod -> Request -> Request
setMethod m req = let !nMethod = renderStdMethod m in req { method = nMethod }
