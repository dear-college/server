{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App
  ( api,
    server,
    theApplicationWithSettings,
    TheAPI,
  )
where

import AppM (AppCtx (..), AppM (..), HasConfiguration (..), MonadDB (..))
import Configuration
  ( Configuration (getHostname),
    defaultConfiguration,
    updateGithubAccessToken,
    updateGithubRoot,
    updateHostname,
  )
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TVar
  ( TVar,
    newTVar,
    readTVar,
    writeTVar,
  )
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Data.Pool (Pool, createPool, withResource)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Redis as R
import Network.Wai.Handler.Warp
  ( Port (..),
    Settings (..),
    defaultSettings,
    getPort,
    runSettings,
    setLogger,
    setPort,
  )
import qualified Repos
import Servant
import Servant.Server
import System.Environment (lookupEnv)

type TheAPI = Repos.API

-- Repos.API :<|>

api :: Proxy TheAPI
api = Proxy

server ::
  ( MonadIO m, -- MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m
  ) =>
  ServerT TheAPI m
server = Repos.server

-- User.server :<|>

nt :: AppCtx -> AppM a -> Handler a
nt s x = runReaderT (runApp x) s

appWithContext :: AppCtx -> IO Application
appWithContext ctx = do
  let pool = getPool ctx
  withResource pool $ \conn -> do
    let cfg = conn :. EmptyContext
    pure $ serveWithContext api cfg $ hoistServerWithContext api (Proxy :: Proxy '[R.Connection]) (nt ctx) server

theApplicationWithSettings :: Settings -> IO Application
theApplicationWithSettings settings = do
  -- with this, lookupEnv will fetch from .env or from an environment variable
  _ <- Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  hostname <- lookupEnv "HOSTNAME"

  root <- lookupEnv "GITHUB_ROOT"
  accessToken <- lookupEnv "GITHUB_ACCESS_TOKEN"
  let config = updateGithubRoot root $ updateGithubAccessToken accessToken $ updateHostname hostname $ defaultConfiguration

  putStrLn $ "Listening on port " ++ show (getPort settings)

  redisConnectionSocket <- lookupEnv "REDIS_SOCKET"
  let f s = Right R.defaultConnectInfo {R.connectPort = R.UnixSocket s}
  let connectSocket = maybe (Right R.defaultConnectInfo) f redisConnectionSocket

  redisConnectionString <- lookupEnv "REDIS"
  let connectInfo = maybe connectSocket R.parseConnectInfo redisConnectionString

  let connectInfo' = case connectInfo of
        Left e -> error e
        Right c -> c

  pool <-
    createPool
      (R.checkedConnect connectInfo') -- creating connection
      (\conn -> void $ R.runRedis conn R.quit) -- clean-up action
      1 -- number of sub-pools
      60 -- how long in seconds to keep unused connections open
      50 -- maximum number of connections
  conn <- either error R.checkedConnect connectInfo

  let context = AppCtx {_getConfiguration = config, getPool = pool}

  appWithContext context
