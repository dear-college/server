{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AppM
  ( AppCtx (..),
    AppM (..),
    MonadDB (..),
    Key,
    Field,
    HasConfiguration (..),
    HasOidcEnvironment (..),
    HasJwtSettings (..),
    HasCookieSettings (..)    
  )
where

import Configuration
  ( Configuration (getHostname),
  )
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch (MonadCatch, MonadThrow(..), catch)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.ByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Pool (Pool, withResource)
import qualified Database.Redis as R
import Network.Wai.Handler.Warp
import Servant.Server
import OIDC.Types ( OIDCEnv(..) )
import Web.OIDC.Client.Types (SessionStore)

import Crypto.JOSE
import Servant.Auth.Server

data AppCtx = AppCtx
  { _getConfiguration :: Configuration
  , getPool :: Pool R.Connection
  , _getOidcEnvironment :: OIDCEnv
  , _getSymmetricJWK :: JWK
  , _jwtSettings :: JWTSettings
  , _cookieSettings :: CookieSettings
  }

type Key = ByteString

type Field = ByteString

class MonadDB m where
  hset :: Key -> Field -> ByteString -> m (Either R.Reply Integer)
  hget :: Key -> Field -> m (Either R.Reply (Maybe ByteString))
  rget :: Key -> m (Either R.Reply (Maybe ByteString))
  rset :: Key -> ByteString -> m (Either R.Reply R.Status)
  rsetex :: Key -> Integer -> ByteString -> m (Either R.Reply R.Status)
  rexists :: Key -> m (Either R.Reply Bool)
  expire :: Key -> Integer -> m (Either R.Reply Bool)  
  zadd :: Key -> Double -> Field -> m (Either R.Reply Integer)
  zscore :: Key -> Field -> m (Either R.Reply (Maybe Double))
  setex :: Key -> Integer -> ByteString -> m (Either R.Reply R.Status)
  setex key _ = rset key

newtype AppM a = AppM {runApp :: ReaderT AppCtx Servant.Server.Handler a}
  deriving (Functor, Applicative, MonadReader AppCtx, Monad, MonadIO, MonadError ServerError)

instance MonadDB AppM where
  hset key field value = withConnectionAndRun (R.hset key field value)
  hget key field = withConnectionAndRun (R.hget key field)
  rget key = withConnectionAndRun (R.get key)
  rset key value = withConnectionAndRun (R.set key value)
  rsetex key seconds value = withConnectionAndRun (R.setex key seconds value)
  expire key seconds = withConnectionAndRun (R.expire key seconds)
  rexists key = withConnectionAndRun (R.exists key)
  zadd key double field = withConnectionAndRun (R.zadd key [(double, field)])
  zscore key field = withConnectionAndRun (R.zscore key field)

withConnectionAndRun :: R.Redis a -> AppM a
withConnectionAndRun f = AppM $ do
  pool <- asks getPool
  liftIO $ withResource pool $ \conn -> R.runRedis conn f

class HasConfiguration a where
  getConfiguration :: a -> Configuration

instance HasConfiguration AppCtx where
  getConfiguration = _getConfiguration

class HasOidcEnvironment a where
  getOidcEnvironment :: a -> OIDCEnv

instance HasOidcEnvironment AppCtx where
  getOidcEnvironment = _getOidcEnvironment

class HasJwtSettings a where
  getJwtSettings :: a -> JWTSettings

instance HasJwtSettings AppCtx where
  getJwtSettings = _jwtSettings

class HasCookieSettings a where
  getCookieSettings :: a -> CookieSettings

instance HasCookieSettings AppCtx where
  getCookieSettings = _cookieSettings

instance MonadThrow AppM where
    throwM = AppM . liftIO . throwM

instance MonadCatch AppM where
    catch (AppM m) handler = AppM $ m `catch` (runApp . handler)
