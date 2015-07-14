{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Network.HTTP.Client.Free.Examples (
) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Data.Time (UTCTime(UTCTime), fromGregorian)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import           Network.HTTP.Types.Method (StdMethod (..))
import Network.HTTP.Types.Version (http09, http10, http11, HttpVersion)
import Network.HTTP.Client (defaultManagerSettings, newManager, parseUrl, responseStatus, Request)
import Network.HTTP.Client.Free (get)
import qualified Network.HTTP.Client.Free.ArbitraryClient as ArbitraryClient
import Network.HTTP.Client.Free.HttpClient (HttpClient)
import qualified Network.HTTP.Client.Free.HttpClient as HttpClient
import Network.HTTP.Client.Free.Types (FreeHttp, RequestType, ResponseType)
import Network.HTTP.Client.Internal (Cookie(Cookie), CookieJar, createCookieJar, Response(Response), ResponseClose(ResponseClose))
import           Test.QuickCheck           (choose, Gen, Arbitrary(arbitrary), elements, listOf, sample', suchThat)

-- | an arbitrary 'Status'
arbStatus :: Gen Status
arbStatus = elements [ status100
                     , status101
                     , status200
                     , status201
                     , status203
                     , status204
                     , status205
                     , status206
                     , status300
                     , status301
                     , status302
                     , status303
                     , status304
                     , status305
                     , status307
                     , status400
                     , status401
                     , status402
                     , status403
                     , status404
                     , status405
                     , status406
                     , status407
                     , status408
                     , status409
                     , status410
                     , status411
                     , status412
                     , status413
                     , status414
                     , status415
                     , status416
                     , status417
                     , status418
                     , status428
                     , status429
                     , status431
                     , status500
                     , status501
                     , status502
                     , status503
                     , status504
                     , status505
                     , status511
                     ]

-- | an arbitrary 'HttpVersion'
arbHttpVersion :: Gen HttpVersion
arbHttpVersion = elements [ http09
                          , http10
                          , http11
                          ]

-- | an arbitrary 'HeaderName'
arbHeaderName :: Gen HeaderName
arbHeaderName = elements [ hAccept
                         , hAcceptLanguage
                         , hAuthorization
                         , hCacheControl
                         , hConnection
                         , hContentEncoding
                         , hContentLength
                         , hContentMD5
                         , hContentType
                         , hCookie
                         , hDate
                         , hIfModifiedSince
                         , hIfRange
                         , hLastModified
                         , hLocation
                         , hRange
                         , hReferer
                         , hServer
                         , hUserAgent
                         ]

-- | an arbitrary Header. This is not performant, but you shouldn't
-- be using this client in production anyway.
arbHeader :: Gen Header
arbHeader = (,) <$> arbHeaderName <*> fmap pack arbitrary

-- | an arbitrary UTCTime
arbUtcTime :: Gen UTCTime
arbUtcTime = do
    rDay <- choose (1,29) :: Gen Int
    rMonth <- choose (1,12) :: Gen Int
    rYear <- choose (1970, 2015) :: Gen Integer
    rTime <- choose (0,86401) :: Gen Int
    return $ UTCTime (fromGregorian rYear rMonth rDay) (fromIntegral rTime)

-- | an arbtirary Cookie
arbCookie :: Gen Cookie
arbCookie = do
    cCreationTime    <- arbUtcTime
    cLastAccessTime  <- suchThat arbUtcTime (cCreationTime >=)
    cExpiryTime      <- suchThat arbUtcTime (cLastAccessTime >=)
    cName           <- fmap pack arbitrary
    cValue          <- fmap pack arbitrary
    cDomain         <- fmap pack arbitrary
    cPath           <- fmap pack arbitrary
    cPersistent     <- arbitrary
    cHostOnly       <- arbitrary
    cSecureOnly     <- arbitrary
    cHttpOnly       <- arbitrary
    return $ Cookie cName
                    cValue
                    cExpiryTime
                    cDomain
                    cPath
                    cCreationTime
                    cLastAccessTime
                    cPersistent
                    cHostOnly
                    cSecureOnly
                    cHttpOnly

-- | unexported instance for arbitrary responses
instance Arbitrary (Response ByteString) where
    arbitrary = Response <$> arbStatus
                         <*> arbHttpVersion
                         <*> listOf arbHeader
                         <*> (pack <$> arbitrary)
                         <*> (createCookieJar <$> listOf arbCookie)
                         <*> return (ResponseClose (return ()))

-- | A sample request
weirdReq :: Request
weirdReq = fromJust (parseUrl "http://weirdcanada.com/api")

-- | A program that checks to see if the weird canada api is up.
checkWeird :: ( Request ~ RequestType client
              , Response b ~ ResponseType client
              , Monad m
              )
           => FreeHttp client m Bool
checkWeird = do
    resp <- get weirdReq
    (return . (== status200) . responseStatus) resp

data ExampleClient
type instance RequestType ExampleClient = Request
type instance ResponseType ExampleClient = Response ByteString

main :: IO ()
main = do
    -- a result using the arbitrary interpreter
    arbResult <- ArbitraryClient.runHttp () (checkWeird :: FreeHttp ExampleClient IO Bool)
    putStrLn ("Arbitrary client returned: " ++ show arbResult)

    -- a result using the actual http client
    mgr <- newManager defaultManagerSettings
    realResult <- HttpClient.runHttp mgr (checkWeird :: FreeHttp HttpClient IO Bool)

    putStrLn ("http-client returned: " ++ show realResult)



