{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

import AppM
  ( AppCtx (..),
    AppM (..),
    HasConfiguration (..),
    HasCookieSettings (..),
    HasJwtSettings (..),
    HasOidcEnvironment (..),
    MonadDB (..),
  )
import Configuration
  ( Configuration (getRootURI),
    defaultConfiguration,
    updateGithubAccessToken,
    updateGithubRoot,
    updateRootURI,
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
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Crypto.JOSE.JWK (JWK)
import qualified Crypto.JOSE.JWK as Jose
import Crypto.JOSE.Types (Base64Octets (..))
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
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
import qualified OIDC
import OIDC.Types
  ( OIDCConf (..),
    genRandomBS,
    initOIDC,
  )
import qualified Repos
import Servant
import Servant.Auth.Server as SAS
import Servant.Server
import System.Environment (lookupEnv)
import Network.URI (URI (..), parseURI)

type TheAPI = Repos.API :<|> OIDC.API

-- Repos.API :<|>

api :: Proxy TheAPI
api = Proxy

server ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    HasOidcEnvironment r,
    HasJwtSettings r,
    HasCookieSettings r,
    MonadError ServerError m,
    MonadCatch m
  ) =>
  ServerT TheAPI m
server = Repos.server :<|> OIDC.server

nt :: AppCtx -> AppM a -> Servant.Server.Handler a
nt s x = runReaderT (runApp x) s

appWithContext :: AppCtx -> IO Application
appWithContext ctx = do
  let pool = getPool ctx
      myKey = _getSymmetricJWK ctx
      jwtCfg = defaultJWTSettings myKey
  withResource pool $ \conn -> do
    let cookies = defaultCookieSettings { cookieXsrfSetting = Nothing }
    let cfg = conn :. jwtCfg :. cookies :. EmptyContext
    let ctx' = ctx {_jwtSettings = jwtCfg, _cookieSettings = cookies}
    pure $
      serveWithContext api cfg $
        hoistServerWithContext
          api
          (Proxy :: Proxy '[R.Connection, SAS.CookieSettings, SAS.JWTSettings])
          (nt ctx')
          server

-- | Generate a key suitable for use with 'defaultConfig' using file contents
generateKeyFromFile :: String -> IO JWK
generateKeyFromFile filename = do
  keyContents <- BS.readFile filename
  let keyContentsEncoded = B64URL.encode keyContents
  let keyParams = Jose.OctKeyMaterial . Jose.OctKeyParameters . Base64Octets $ keyContentsEncoded
  return $ Jose.fromKeyMaterial keyParams

theApplicationWithSettings :: Settings -> IO Application
theApplicationWithSettings settings = do
  -- with this, lookupEnv will fetch from .env or from an environment variable
  _ <- Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  rootURI' <- lookupEnv "ROOT_URL"
  rootURI <- maybe (error "Could not parse $ROOT_URL") (pure . parseURI) rootURI'

  symmetricKeyFilename <- lookupEnv "SYMMETRIC_KEY"
  symmetricJwk <- maybe (error "Expected the file $SYMMETRIC_KEY to contain symmetric key material") generateKeyFromFile symmetricKeyFilename

  root <- lookupEnv "GITHUB_ROOT"
  accessToken <- lookupEnv "GITHUB_ACCESS_TOKEN"

  let packLookupEnv x = fmap C8.pack <$> lookupEnv x
  googleClientId <- packLookupEnv "GOOGLE_CLIENT_ID"
  googleClientSecret <- packLookupEnv "GOOGLE_CLIENT_SECRET"
  googleRedirectUri <- packLookupEnv "GOOGLE_REDIRECT_URI"

  let oidcConf = OIDCConf <$> googleRedirectUri <*> googleClientId <*> googleClientSecret
  oidcEnv <- maybe (error "Missing GOOGLE_* in .env") initOIDC oidcConf

  let config =
        updateGithubRoot root $
          updateGithubAccessToken accessToken $
            updateRootURI rootURI $
              defaultConfiguration

  putStrLn $ "Listening on port " ++ show (getPort settings)

  redisConnectionSocket <- lookupEnv "REDIS_SOCKET"
  let f s = Right R.defaultConnectInfo {R.connectPort = R.UnixSocket s}
  let connectSocket = maybe (Right R.defaultConnectInfo) f redisConnectionSocket

  redisConnectionString <- lookupEnv "REDIS"
  let connectInfo = maybe connectSocket R.parseConnectInfo redisConnectionString

  let connectInfo' = case connectInfo of
        Left e -> error e
        Right c -> c

  let poolConfig = defaultPoolConfig
        (R.checkedConnect connectInfo') -- creating connection
        (\conn -> void $ R.runRedis conn R.quit) -- clean-up action
        60 -- how long in seconds to keep unused connections open
        50 -- maximum number of connections

  pool <- newPool poolConfig
  conn <- either error R.checkedConnect connectInfo

  let context = AppCtx {_getConfiguration = config, getPool = pool, _getSymmetricJWK = symmetricJwk, _getOidcEnvironment = oidcEnv}

  appWithContext context
