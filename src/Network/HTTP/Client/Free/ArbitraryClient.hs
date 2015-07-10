{-| An interpreter that fails randomly
-}

module Network.HTTP.Client.Free.ArbitraryClient where

import Data.ByteString.Char8 (pack)
import Data.Time (UTCTime(UTCTime), fromGregorian)
import Control.Applicative ((<$>), (<*>))
import           Network.HTTP.Types.Method (StdMethod (..))
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version (http09, http10, http11, HttpVersion)
import Network.HTTP.Client.Internal (Cookie(Cookie), Response)
import           Test.QuickCheck           (choose, Gen, Arbitrary(arbitrary), elements, suchThat)

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
    cLastAccessTime  <- suchThat arbUtcTime (cCreationTime <=)
    cExpiryTime      <- suchThat arbUtcTime (cLastAccessTime <=)
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


-- instance Arbitrary body => Arbitrary (Response body) where
    -- arbitrary = Arbitrary <$> 
