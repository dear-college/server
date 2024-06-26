{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    HasUser (..),
    MonadDB (..),
    MonadTime (..),
  )
import qualified Backend
import Configuration
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import qualified Courses
import qualified Crypto.JOSE.JWK as Jose
import Crypto.JOSE.Types (Base64Octets (..))
import Crypto.JWT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as LazyByteString
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as ByteString
import qualified Data.Maybe
import Data.Pool (defaultPoolConfig, newPool, withResource)
import qualified Database.Redis as R
import FindFile (findFirstFileWithExtension)
import qualified Markdown
import qualified Favicon
import Network.URI (parseURI)
import Network.Wai (Request (..), requestHeaders)
import Network.Wai.Handler.Warp
  ( Settings,
    getPort,
  )
import qualified OIDC
import OIDC.Types (OIDCConf (..), initOIDC)
import Servant
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

type TheAPI = OIDC.API :<|> Markdown.API :<|> Courses.API :<|> Backend.API :<|> Favicon.API :<|> ("assets" :> Raw) 

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
  FilePath ->
  ServerT TheAuthAPI m
server assetPath markdownPath faviconPath user =
  hoistServerWithContext
    (Proxy :: Proxy TheAPI)
    proxyCtx
    (ntUser user)
    $ OIDC.server :<|> (Markdown.server markdownPath) :<|> Courses.server :<|> Backend.server :<|> (Favicon.server faviconPath) :<|> serveDirectoryWebApp assetPath

-- https://nicolasurquiola.ar/blog/2023-10-28-generalised-auth-with-jwt-in-servant
type AuthJwtCookie = AuthProtect "jwt-cookie"

-- https://hackage.haskell.org/package/biscuit-servant-0.3.0.1/docs/Auth-Biscuit-Servant.html

authHandler :: JWK -> JWTValidationSettings -> AuthHandler Request User
authHandler key settings = mkAuthHandler handler
  where
    handler req = case lookup "Cookie" $ requestHeaders req of
      Nothing -> pure Unauthenticated
      Just cookies -> case lookup "JWT-Cookie" $ parseCookies cookies of
        Nothing -> pure Unauthenticated
        Just token -> do
          (jwt :: Maybe ClaimsSet) <- liftIO $ verifyToken key settings token
          case jwt of
            Nothing -> pure Unauthenticated
            Just claims -> do
              case preview (claimSub . _Just . uri) claims of
                Nothing -> pure Unauthenticated
                Just s -> do
                  pure $ AuthenticatedUser $ Subscriber s

verifyToken :: JWK -> JWTValidationSettings -> ByteString -> IO (Maybe ClaimsSet)
verifyToken key settings token = do
  x <- runJOSE @JWTError verifyJwt
  case x of
    Left _ -> return Nothing
    Right m -> return $ Just m
  where
    verifyJwt = do
      c <- decodeCompact lazyToken
      verifyClaims settings key c

    lazyToken = LazyByteString.fromString (ByteString.toString token)

type instance AuthServerData AuthJwtCookie = User

nt :: AppCtx -> AppM a -> Servant.Server.Handler a
nt s x = runReaderT (runApp x) s

appWithContext :: FilePath -> FilePath -> FilePath -> AppCtx -> IO Application
appWithContext assetPath markdownPath faviconPath ctx = do
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
          (server assetPath markdownPath faviconPath)

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

  frontendPath <- lookupEnv "FRONTEND_PATH"
  let assetsDirectory = Data.Maybe.fromMaybe (error "Missing FRONTEND_PATH in .env") frontendPath

  markdownPath <- lookupEnv "MARKDOWN_PATH"
  let markdownDirectory = Data.Maybe.fromMaybe (error "Missing MARKDOWN_PATH in .env") markdownPath

  faviconPath <- lookupEnv "FAVICON_PATH"
  let faviconDirectory = Data.Maybe.fromMaybe (error "Missing FAVICON_PATH in .env") faviconPath
  
  mJsPath <- findFirstFileWithExtension assetsDirectory ".js"
  let jsFilename = maybe (error "Could not find .js file in assets") takeFileName mJsPath

  mCssPath <- findFirstFileWithExtension assetsDirectory ".css"
  let cssFilename = maybe (error "Could not find .css file in assets") takeFileName mCssPath

  google <- lookupEnv "GOOGLE_ANALYTICS"

  let config =
        updateRootURI rootURI $
          updateJavascriptPath (Just jsFilename) $
            updateStylesheetPath (Just cssFilename) $
              updateGoogleAnalytics google defaultConfiguration

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

  let context =
        AppCtx
          { _getConfiguration = config,
            getPool = pool,
            _getSymmetricJWK = symmetricJwk,
            _getOidcEnvironment = oidcEnv
          }

  appWithContext assetsDirectory markdownDirectory faviconDirectory context
