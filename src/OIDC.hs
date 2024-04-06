{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OIDC (API (..), server) where

import AppM (AppM, HasConfiguration (..), HasCookieSettings (..), HasJwtSettings (..), HasOidcEnvironment (..), MonadDB (..), getConfiguration, getPool)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
  ( FromJSON (..),
    (.:),
  )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as AeT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Client
  ( Manager,
    newManager,
  )
import Network.HTTP.Client.TLS
  ( tlsManagerSettings,
  )
import OIDC.Types (OIDCConf (..), OIDCEnv (..), genRandomBS)
import Servant
import Servant.Auth.Server as SAS
import Servant.HTML.Blaze
  ( HTML,
  )
import Servant.Server
import qualified System.Random as Random
import Text.Blaze
  ( ToMarkup (..),
  )
import qualified Text.Blaze.Html as H
import Text.Blaze.Html5 (ToMarkup, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Web.OIDC.Client as O
import Web.OIDC.Client.Tokens
import Web.OIDC.Client.Tokens
  ( IdTokenClaims (..),
    Tokens (..),
    validateIdToken,
  )
import Web.OIDC.Client.Types (Code, SessionStore (..), State)

instance FromHttpApiData C8.ByteString where
  parseUrlPiece = Right . C8.pack . Data.Text.unpack
  parseHeader = Right
  parseQueryParam = Right . C8.pack . Data.Text.unpack

type API =
  "login"
    :> ( -- redirect User to the OpenID Provider
         Get '[JSON] NoContent
           -- render the page that will save the user creds in the user-agent
           :<|> ("cb" :> QueryParam "error" Text :> QueryParam "code" Code :> QueryParam "state" State :> Get '[HTML] String)
       )

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
  m NoContent
handleLogin = do
  oidcenv <- asks getOidcEnvironment
  let conf = oidcConf oidcenv
  let oidcCreds = O.setCredentials (clientId conf) (clientSecret conf) (redirectUri conf) (O.newOIDC $ prov oidcenv)
  loc <- O.prepareAuthenticationRequestUrl sessionStore oidcCreds [O.openId, O.email, O.profile] []
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

handleLoggedIn ::
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
  -- | handle successful id
  LoginHandler ->
  -- | error
  Maybe Text ->
  -- | code
  Maybe Code ->
  -- | state
  Maybe State ->
  m String
handleLoggedIn handleSuccessfulId err mcode mstate = do
  oidcenv <- asks getOidcEnvironment
  state <- maybe (forbidden "Missing state") pure mstate
  code <- maybe (forbidden "Missing code") pure mcode
  case err of
    Just errorMsg -> forbidden errorMsg
    Nothing -> do
      tokens <- O.getValidTokens sessionStore (oidc oidcenv) (mgr oidcenv) state code :: (MonadIO m, MonadDB m, MonadCatch m) => m (Tokens OtherClaims)
      return $ show (idToken tokens)

-- case err of
--   Just errorMsg -> forbidden errorMsg
--   Nothing -> case mcode of
--     Just oauthCode -> do
--       tokens <- liftIO $ O.requestTokens (oidc oidcenv) Nothing (encodeUtf8 oauthCode) (mgr oidcenv)
--       --putText . show . O.claims . O.idToken $ tokens
--       --let jwt = toS . unJwt . O.jwt . O.idToken $ tokens
--       let jwt = O.idToken tokens
--       let eAuthInfo = O.validateIdToken :: Either O.JwtError (O.JwtHeader,AuthInfo)
--       case eAuthInfo of
--         Left jwtErr -> forbidden $ "JWT decode/check problem: " <> show jwtErr
--         Right (_,authInfo) ->
--           if emailVerified authInfo
--             then do
--               user <- liftIO $ handleSuccessfulId authInfo
--               either forbidden return user
--             else forbidden "Please verify your email"
--     Nothing -> do
--       liftIO $ putStrLn "No code param"
--       forbidden "no code parameter given"

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
  ServerT API m
server = handleLogin :<|> (handleLoggedIn loginHandler)

-- server :: OIDCEnv -> LoginHandler -> Server API
-- server oidcenv loginHandler =
--  handleLogin oidcenv :<|> handleLoggedIn oidcenv loginHandler

type APIKey = BS.ByteString

type Account = Text

type Conf = [(APIKey, Account)]

data Customer = Customer
  { account :: Account,
    apiKey :: APIKey,
    mail :: Maybe Text,
    fullname :: Maybe Text
  }

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
