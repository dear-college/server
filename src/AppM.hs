{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM (AppCtx(..)
            , AppM(..)
            , MonadDB(..)
            , Key
            , Field
            , HasConfiguration(..)
            ) where

import qualified Database.Redis as R
import Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Servant.Server
import Data.Pool (Pool, withResource)

import Data.ByteString
import qualified Data.Map as Map
import Data.Map (Map)

import Configuration
    ( Configuration(getHostname) )

import Network.Wai.Handler.Warp

data AppCtx = AppCtx { _getConfiguration :: Configuration
                     , getPool :: Pool R.Connection
                     }

type Key = ByteString
type Field = ByteString

class MonadDB m where
  hset :: Key -> Field -> ByteString -> m (Either R.Reply Integer)
  hget :: Key -> Field -> m (Either R.Reply (Maybe ByteString))
  rget :: Key -> m (Either R.Reply (Maybe ByteString))
  rset :: Key -> ByteString -> m (Either R.Reply R.Status)
  zadd :: Key -> Double -> Field -> m (Either R.Reply Integer)
  zscore :: Key -> Field -> m (Either R.Reply (Maybe Double))
  setex :: Key -> Integer -> ByteString -> m (Either R.Reply R.Status)
  setex key _ = rset key

newtype AppM a = AppM { runApp :: ReaderT AppCtx Handler a }
   deriving (Functor, Applicative, MonadReader AppCtx, Monad, MonadIO, MonadError ServerError)

-- instance MonadDB AppM where
--   hset key field value = withConnectionAndRun (R.hset key field value)
--   hget key field = withConnectionAndRun (R.hget key field)
--   rget key = withConnectionAndRun (R.get key)
--   rset key value = withConnectionAndRun (R.set key value)
--   zadd key double field = withConnectionAndRun (R.zadd key [(double,field)])
--   zscore key field = withConnectionAndRun (R.zscore key field)

-- withConnectionAndRun :: R.Redis a -> AppM a
-- withConnectionAndRun f = AppM $ do
--   pool <- asks getPool
--   withResource pool $ \conn -> runApp $ do
--     liftIO $ R.runRedis conn f

class HasConfiguration a where
  getConfiguration :: a -> Configuration

instance HasConfiguration AppCtx where
  getConfiguration = _getConfiguration



