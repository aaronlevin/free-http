{-| An interpreter that fails randomly
-}

{-# LANGUAGE TypeFamilies #-}

module Network.HTTP.Client.Free.ArbitraryClient where

import Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans             (MonadTrans, lift)
import           Control.Monad.Trans.Free.Church (FT, iterT, iterTM, liftF)
import Control.Applicative ((<$>))
import Network.HTTP.Client.Free.Types (HttpF(HttpF), ResponseType)
import           Test.QuickCheck           (Arbitrary, arbitrary, sample')

-------------------------------------------------------------------------------
-- | Peel a layer of the 'HttpF' functor and generate a random Response.
iterTHttp :: ( r ~ ResponseType client
             , Arbitrary r
             , Monad m
             , MonadIO m
             )
          => HttpF client (m a)
          -> m a
iterTHttp (HttpF _ _ next) = head <$> liftIO (sample' arbitrary) >>= next

-------------------------------------------------------------------------------
-- | Peel a layer of the 'HttpF' functor and generate a random Response. This 
-- time the base monad is 't m'.
iterTMHttp :: ( r ~ ResponseType client
              , Arbitrary r
              , Monad m
              , MonadIO m
              , MonadTrans t
              , Monad (t m)
              )
           => HttpF client (t m a)
           -> t m a
iterTMHttp (HttpF _ _ next) = head <$> (lift . liftIO) (sample' arbitrary) >>= next

-------------------------------------------------------------------------------
-- | The main http-client interpreter. The client is free to specify the base
-- effect monad so long as there is an instance of 'MonadIO' for it in scope.
runHttp :: ( r ~ ResponseType client
           , Arbitrary r
           , Monad m
           , MonadIO m
           )
        => ignore
        -- ^ a paramter that will be ignored. It is included so client's can
        -- hot-swap interpreters.
        -> FT (HttpF client) m a
        -> m a
runHttp = const (iterT iterTHttp)

-------------------------------------------------------------------------------
-- | The main http-client interpreter. The client is free to specify the base
-- effect monad ('m'), and in thise case this the result can be lifted into a
-- higher monad transformer stack ('t')
runTHttp :: ( r ~ ResponseType client
            , Arbitrary r
            , Monad m
            , MonadIO m
            , MonadTrans t
            , Monad (t m)
            )
         => ignore
         -- ^ a paramter that will be ignored. It is included so client's can
         -- hot-swap interpreters.
         -> FT (HttpF client) m a
         -> t m a
runTHttp = const (iterTM iterTMHttp)
