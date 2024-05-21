{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

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
    HasUser (..),
    MonadDB (..),
    MonadRandom (..),
    MonadTime (..),
  )
import Auth
-- import Crypto.JWT (HasClaimsSet, JWTError, JWTValidationSettings, SignedJWT, signJWT, verifyJWT, defaultJWTValidationSettings, claimSub, string, verifyClaims)
import qualified Backend
import Configuration
  ( Configuration (getRootURI),
    defaultConfiguration,
    updateJavascriptPath,
    updateRootURI,
    updateStylesheetPath,
  )
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (bracket)
import Control.Lens
import Control.Lens.Prism
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Courses
import Crypto.JOSE
  ( JOSE,
    JWK,
    KeyMaterialGenParam (OctGenParam),
    bestJWSAlg,
    decodeCompact,
    genJWK,
    newJWSHeader,
    runJOSE,
  )
import Crypto.JOSE.JWK (JWK)
import qualified Crypto.JOSE.JWK as Jose
import Crypto.JOSE.Types (Base64Octets (..))
import Crypto.JWT
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as LazyByteString
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as ByteString
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Redis as R
import FindFile (findFirstFileWithExtension)
import qualified Markdown
import Network.URI (URI (..), parseURI)
import Network.Wai (Request (..), requestHeaders)
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
import OIDC.Types (OIDCConf (..), initOIDC)
import qualified Repos
import Servant
import Servant.API.Experimental.Auth (AuthProtect)
import qualified Servant.Auth.Server as SAS
import Servant.Auth.Server.Internal.ConfigTypes
import Servant.Server
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    AuthServerData,
    mkAuthHandler,
  )
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)
import User
import Web.Cookie (parseCookies)

ntUser :: forall a r m. (MonadReader r m, HasUser r) => User -> m a -> m a
ntUser user = local (putUser user)

proxyCtx :: Proxy '[AuthHandler Request User, R.Connection, SAS.CookieSettings, SAS.JWTSettings]
proxyCtx = Proxy

type TheAPI = Repos.API :<|> OIDC.API :<|> Markdown.API :<|> Courses.API :<|> Backend.API -- :<|> Raw

type TheAuthAPI = AuthJwtCookie :> TheAPI

api :: Proxy TheAuthAPI
api = Proxy

server ::
  ( MonadIO m,
    MonadDB m,
    MonadTime m,
    MonadReader r m,
    HasConfiguration r,
    HasUser r,
    HasOidcEnvironment r,
    HasJwtSettings r,
    HasCookieSettings r,
    MonadError ServerError m,
    MonadCatch m,
    MonadRandom m
  ) =>
  FilePath ->
  FilePath ->
  ServerT TheAuthAPI m
server assetPath markdownPath user = do
  hoistServerWithContext
    (Proxy :: Proxy TheAPI)
    proxyCtx
    (ntUser user)
    $ Repos.server :<|> OIDC.server :<|> (Markdown.server markdownPath) :<|> Courses.server :<|> Backend.server -- :<|> serveDirectoryWebApp assetPath

-- https://nicolasurquiola.ar/blog/2023-10-28-generalised-auth-with-jwt-in-servant
type AuthJwtCookie = AuthProtect "jwt-cookie"

-- https://hackage.haskell.org/package/biscuit-servant-0.3.0.1/docs/Auth-Biscuit-Servant.html

authHandler :: JWK -> JWTValidationSettings -> AuthHandler Request User
authHandler jwk settings = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = do
      case lookup "Cookie" $ requestHeaders req of
        Nothing -> pure Unauthenticated
        Just cookies -> case lookup "JWT-Cookie" $ parseCookies cookies of
          Nothing -> pure Unauthenticated
          Just token -> do
            (jwt :: Maybe ClaimsSet) <- liftIO $ verifyToken jwk settings token
            case jwt of
              Nothing -> pure Unauthenticated
              Just claims -> do
                case preview (claimSub . _Just . uri) claims of
                  Nothing -> pure Unauthenticated
                  Just s -> do
                    pure $ AuthenticatedUser $ Subscriber s

maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

verifyToken :: JWK -> JWTValidationSettings -> ByteString -> IO (Maybe ClaimsSet)
verifyToken jwk settings token = do
  x <- runJOSE @JWTError verify
  case x of
    Left e -> do
      return Nothing
    Right m -> do
      return $ Just m
  where
    verify = do
      c <- decodeCompact lazy
      verifyClaims settings jwk c

    lazy = LazyByteString.fromString (ByteString.toString token)

type instance AuthServerData AuthJwtCookie = User

nt :: AppCtx -> AppM a -> Servant.Server.Handler a
nt s x = runReaderT (runApp x) s

appWithContext :: FilePath -> FilePath -> AppCtx -> IO Application
appWithContext assetPath markdownPath ctx = do
  let pool = getPool ctx
      myKey = _getSymmetricJWK ctx
      jwtCfg = SAS.defaultJWTSettings myKey
  withResource pool $ \conn -> do
    let cookies = SAS.defaultCookieSettings {SAS.cookieXsrfSetting = Nothing}
    let cfg = authHandler myKey (jwtSettingsToJwtValidationSettings jwtCfg) :. conn :. jwtCfg :. cookies :. EmptyContext
    let ctx' = ctx {_jwtSettings = jwtCfg, _cookieSettings = cookies}

    pure $
      serveWithContext api cfg $
        hoistServerWithContext
          api
          proxyCtx
          (nt ctx')
          (server assetPath markdownPath)

-- | Generate a key suitable for use with 'defaultConfig' using file contents
generateKeyFromFile :: String -> IO JWK
generateKeyFromFile filename = do
  keyContents <- BS.readFile filename
  let keyContentsEncoded = B64URL.encode keyContents
  let keyParams = Jose.OctKeyMaterial . Jose.OctKeyParameters . Base64Octets $ keyContentsEncoded
  return $ Jose.fromKeyMaterial keyParams

theApplicationWithSettings :: Settings -> IO Application
theApplicationWithSettings settings = do
  rootURI' <- lookupEnv "ROOT_URL"
  rootURI <- maybe (error "Could not parse $ROOT_URL") (pure . parseURI) rootURI'

  symmetricKeyFilename <- lookupEnv "SYMMETRIC_KEY"
  symmetricJwk <- maybe (error "Expected the file $SYMMETRIC_KEY to contain symmetric key material") generateKeyFromFile symmetricKeyFilename

  let packLookupEnv x = fmap C8.pack <$> lookupEnv x
  googleClientId <- packLookupEnv "GOOGLE_CLIENT_ID"
  googleClientSecret <- packLookupEnv "GOOGLE_CLIENT_SECRET"
  googleRedirectUri <- packLookupEnv "GOOGLE_REDIRECT_URI"

  let oidcConf = OIDCConf <$> googleRedirectUri <*> googleClientId <*> googleClientSecret
  oidcEnv <- maybe (error "Missing GOOGLE_* in .env") initOIDC oidcConf

  rootURI' <- lookupEnv "ROOT_URL"

  frontendPath <- lookupEnv "FRONTEND_PATH"
  let assetsDirectory = maybe (error "Missing FRONTEND_PATH in .env") id frontendPath

  markdownPath <- lookupEnv "MARKDOWN_PATH"
  let markdownDirectory = maybe (error "Missing MARKDOWN_PATH in .env") id markdownPath

  mJsPath <- findFirstFileWithExtension assetsDirectory ".js"
  let jsFilename = maybe (error "Could not find .js file in assets") takeFileName mJsPath

  mCssPath <- findFirstFileWithExtension assetsDirectory ".css"
  let cssFilename = maybe (error "Could not find .css file in assets") takeFileName mCssPath

  let config =
        updateRootURI rootURI $
          updateJavascriptPath (Just jsFilename) $
            updateStylesheetPath (Just cssFilename) $
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

  let poolConfig =
        defaultPoolConfig
          (R.checkedConnect connectInfo') -- creating connection
          (\conn -> void $ R.runRedis conn R.quit) -- clean-up action
          60 -- how long in seconds to keep unused connections open
          50 -- maximum number of connections
  pool <- newPool poolConfig
  conn <- either error R.checkedConnect connectInfo

  let context =
        AppCtx
          { _getConfiguration = config,
            getPool = pool,
            _getSymmetricJWK = symmetricJwk,
            _getOidcEnvironment = oidcEnv
          }

  appWithContext assetsDirectory markdownDirectory context
