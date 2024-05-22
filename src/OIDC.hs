{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OIDC (API (..), server) where

import AppM
  ( AppM,
    HasConfiguration (..),
    HasCookieSettings (..),
    HasJwtSettings (..),
    HasOidcEnvironment (..),
    MonadDB (..),
    getConfiguration,
    getPool,
  )
import Configuration (getRootURI)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Error.Lens (throwing, throwing_)
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Crypto.JWT
import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI
  ( parseURI,
    parseURIReference,
    relativeTo,
    uriPath,
    uriToString,
  )
import OIDC.Types (OIDCConf (..), OIDCEnv (..), genRandomBS)
import Servant
import Servant.Auth.Server as SAS
import Servant.Auth.Server.Internal.Cookie
  ( applyCookieSettings,
    applySessionCookieSettings,
    makeSessionCookie,
  )
import Servant.HTML.Blaze (HTML)
import Servant.Server
import Text.Blaze (ToMarkup (..))
import qualified Text.Blaze.Html as H
import Text.Blaze.Html5 (ToMarkup, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Web.Cookie
import qualified Web.OIDC.Client as O
import Web.OIDC.Client.Tokens (IdTokenClaims (..), Tokens (..), validateIdToken)
import Web.OIDC.Client.Types (Code, SessionStore (..), State)

type API =
  ( "login"
      :> ( -- redirect User to the OpenID Provider
           (Header "Referer" URI :> Get '[JSON] NoContent)
             -- render the page that will save the user creds in the user-agent
             :<|> ("cb" :> QueryParam "error" Text :> QueryParam "code" Code :> QueryParam "state" State :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie] NoContent))
         )
  )
    :<|> ("logout" :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie] NoContent))

redirects :: (MonadError ServerError m) => String -> m ()
redirects url = throwError err302 {errHeaders = [("Location", C8.pack url)]}

sessionStore :: (MonadIO m, MonadDB m) => SessionStore m
sessionStore =
  SessionStore
    { sessionStoreGenerate = do liftIO $ genRandomBS,
      sessionStoreSave = \state nonce -> rset state nonce *> expire state 300 *> pure (),
      sessionStoreGet = \state -> rget state >>= return . either (const Nothing) id,
      sessionStoreDelete = return $ ()
    }

handleLogin ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    HasOidcEnvironment r,
    MonadError ServerError m,
    MonadCatch m
  ) =>
  Maybe URI -> m NoContent
handleLogin uri = do
  oidcenv <- asks getOidcEnvironment
  let conf = oidcConf oidcenv
  let oidcCreds = O.setCredentials (clientId conf) (clientSecret conf) (redirectUri conf) (O.newOIDC $ prov oidcenv)

  let store = sessionStore
  state <- sessionStoreGenerate store
  nonce' <- sessionStoreGenerate store
  sessionStoreSave store state nonce'
  let scope = [O.openId, O.email, O.profile]
  loc <- O.getAuthenticationRequestUrl oidcCreds scope (Just state) [("nonce", Just nonce')]

  let uri' = (\u -> C8.pack $ uriToString id u "") <$> uri
  _ <- case uri' of
    Just u -> rset ("redirect:" <> state) u *> expire state 300 *> pure ()
    Nothing -> pure ()
  
  redirects (show loc)
  return NoContent

-- | @AuthInfo@
data AuthInfo = AuthInfo
  { email :: Text,
    emailVerified :: Bool,
    name :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON AuthInfo where
  parseJSON (JSON.Object v) = do
    email :: Text <- v .: "email"
    email_verified :: Bool <- v .: "email_verified"
    name :: Text <- v .: "name"
    return $ AuthInfo (email) email_verified (name)
  parseJSON invalid = AeT.typeMismatch "Coord" invalid

instance JSON.ToJSON AuthInfo where
  toJSON (AuthInfo e ev n) =
    JSON.object
      [ "email" JSON..= (e :: Text),
        "email_verified" JSON..= ev,
        "name" JSON..= (n :: Text)
      ]

type LoginHandler = AuthInfo -> IO (Either Text User)

loginHandler :: AuthInfo -> IO (Either Text User)
loginHandler au = do
  return $ Right $ User "id" "secret" "storage" (Just "url")

data User = User
  { userId :: Text,
    userSecret :: Text,
    localStorageKey :: Text,
    redirectUrl :: Maybe Text
  }
  deriving (Show, Eq, Ord)

data OtherClaims = OtherClaims
  { claimGivenName :: Text,
    claimFamilyName :: Text,
    claimPicture :: Text,
    claimEmail :: Text,
    claimName :: Text,
    claimEmailVerified :: Bool
  }
  deriving (Show, Eq)

instance FromJSON OtherClaims where
  parseJSON = AeT.withObject "OtherClaims" $ \v ->
    OtherClaims
      <$> v .: "given_name"
      <*> v .: "family_name"
      <*> v .: "picture"
      <*> v .: "email"
      <*> v .: "name"
      <*> v .: "email_verified"

-- Thanks to https://nicolasurquiola.ar/blog/2023-10-28-generalised-auth-with-jwt-in-servant
newtype SessionClaims = SessionClaims ClaimsSet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToJWT)

instance HasClaimsSet SessionClaims where
  claimsSet :: Lens' SessionClaims ClaimsSet
  claimsSet f (SessionClaims claims) = SessionClaims <$> f claims

sessionClaims :: URI -> Tokens OtherClaims -> UTCTime -> Maybe SessionClaims
sessionClaims uri tokens expiry = do
  let claims = otherClaims $ idToken tokens
  issuer <- parseURI $ Data.Text.unpack $ iss $ idToken tokens
  subscriber <- parseURIReference $ Data.Text.unpack $ sub $ idToken tokens
  let subAndIss = subscriber `relativeTo` issuer
  subAndIss' <- preview stringOrUri $ uriToString id subAndIss ""
  audience <- preview stringOrUri $ uriToString id uri ""
  pure $
    emptyClaimsSet
      & claimSub ?~ subAndIss'
      & claimAud ?~ Audience [audience]
      & claimExp ?~ NumericDate expiry
      & SessionClaims

instance AsError ServerError where
  _Error = prism' embed match
    where
      embed :: Error -> ServerError
      embed _ = err500
      match :: ServerError -> Maybe Error
      match _ = Nothing

handleLoggedIn ::
  ( MonadIO m,
    MonadDB m,
    MonadRandom m,
    MonadReader r m,
    HasConfiguration r,
    HasOidcEnvironment r,
    HasJwtSettings r,
    HasCookieSettings r,
    MonadError ServerError m,
    MonadCatch m
  ) =>
  -- | handle successful id
  LoginHandler ->
  -- | error
  Maybe Text ->
  -- | code
  Maybe Code ->
  -- | state
  Maybe State ->
  m (Headers '[Header "Set-Cookie" SetCookie] NoContent)
handleLoggedIn handleSuccessfulId err mcode mstate = do
  oidcenv <- asks getOidcEnvironment
  jwtSettings <- asks getJwtSettings
  jwk <- asks getJWK
  cookieSettings <- asks getCookieSettings
  state <- maybe (forbidden "Missing state") pure mstate
  code <- maybe (forbidden "Missing code") pure mcode

  -- TODO: When should the tokens expire?
  issuedAt <- liftIO getCurrentTime
  let expireAt = addUTCTime 86400 issuedAt

  audience <- asks (getRootURI . getConfiguration)

  -- fetch the URL from which we clicked 'login'
  root <- asks (getRootURI . getConfiguration)
  let root' :: C8.ByteString = C8.pack $ uriToString id root ""
  referer <- rget ("redirect:" <> state) >>= return . either (const Nothing) id
  let referer' = fromMaybe root' referer
  
  case err of
    Just errorMsg -> forbidden errorMsg
    Nothing -> do
      tokens <- O.getValidTokens sessionStore (oidc oidcenv) (mgr oidcenv) state code :: (MonadIO m, MonadDB m, MonadCatch m) => m (Tokens OtherClaims)
      let maybeClaims = sessionClaims audience tokens expireAt
      case maybeClaims of
        Nothing -> forbidden "Missing JWT"
        Just claims -> do
          liftIO $ print claims
          bestAlg <- bestJWSAlg jwk
          let bestHeader = newJWSHeader ((), bestAlg)
          mJWT <- signJWT jwk bestHeader claims
          let bs = encodeCompact mJWT
          let cookie =
                applySessionCookieSettings cookieSettings $
                  applyCookieSettings cookieSettings $
                    def {setCookieValue = LBS.toStrict bs}
          throwError $ err302 {errHeaders = [("Location", referer'), ("Set-Cookie", renderSetCookieBS cookie)]}                    
          return $ (addHeader cookie) NoContent

instance ToMarkup User where
  toMarkup User {..} = H.docTypeHtml $ do
    H.head $
      H.title "Logged In"
    H.body $ do
      H.h1 "Logged In"
      H.p (H.toHtml ("Successful login with id " <> userId))
      H.script
        ( H.toHtml
            ( "localStorage.setItem('"
                <> localStorageKey
                <> "','"
                <> userSecret
                <> "');"
                <> "localStorage.setItem('user-id','"
                <> userId
                <> "');"
                <> "window.location='"
                <> fromMaybe "/" redirectUrl
                <> "';" -- redirect the user to /
            )
        )

handleLogout ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    HasOidcEnvironment r,
    MonadError ServerError m,
    MonadCatch m,
    HasCookieSettings r
  ) =>
  m (Headers '[Header "Set-Cookie" SetCookie] NoContent)
handleLogout = do
  cookieSettings <- asks getCookieSettings
  -- TODO: annoyignly, this sets the cookie to "value"
  -- cf. https://github.com/haskell-servant/servant-auth/issues/127
  let clearedSessionCookie = applySessionCookieSettings cookieSettings $ applyCookieSettings cookieSettings def
  -- TODO: A bit frustrating, but I can't seem to redirect and also set a cookie without using throwError?
  throwError $ err302 {errHeaders = [("Location", "/sample"), ("Set-Cookie", renderSetCookieBS clearedSessionCookie)]}

server ::
  ( MonadIO m,
    MonadDB m,
    MonadRandom m,
    MonadReader r m,
    HasConfiguration r,
    HasOidcEnvironment r,
    HasJwtSettings r,
    HasCookieSettings r,
    MonadError ServerError m,
    MonadCatch m
  ) =>
  ServerT API m
server = (handleLogin :<|> (handleLoggedIn loginHandler)) :<|> handleLogout

----------------------------------------------------------------

data Err = Err
  { errTitle :: Text,
    errMsg :: Text
  }

instance ToMarkup Err where
  toMarkup Err {..} = H.docTypeHtml $ do
    H.head $ do
      H.title "Error"
    H.body $ do
      H.h1 (H.a ! HA.href "/" $ "Home")
      H.h2 (H.toHtml errTitle)
      H.p (H.toHtml errMsg)

format :: (ToMarkup a) => a -> LBS.ByteString
format err = toMarkup err & renderMarkup

appToErr :: ServerError -> Text -> ServerError
appToErr x msg =
  x
    { errBody = format (Err (Data.Text.pack (errReasonPhrase x)) msg),
      errHeaders = [("Content-Type", "text/html")]
    }

unauthorized :: (MonadError ServerError m) => Text -> m a
unauthorized = throwError . unauthorizedErr

unauthorizedErr :: Text -> ServerError
unauthorizedErr = appToErr err401

forbidden :: (MonadError ServerError m) => Text -> m a
forbidden = throwError . forbiddenErr

forbiddenErr :: Text -> ServerError
forbiddenErr = appToErr err403

notFound :: (MonadError ServerError m) => Text -> m a
notFound = throwError . notFoundErr

notFoundErr :: Text -> ServerError
notFoundErr = appToErr err404

preconditionFailed :: (MonadError ServerError m) => Text -> m a
preconditionFailed = throwError . preconditionFailedErr

preconditionFailedErr :: Text -> ServerError
preconditionFailedErr = appToErr err412

serverError :: (MonadError ServerError m) => Text -> m a
serverError = throwError . serverErrorErr

serverErrorErr :: Text -> ServerError
serverErrorErr = appToErr err500
