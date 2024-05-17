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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Auth where

import AppM (AppM, HasConfiguration (..), HasCookieSettings (..), HasJwtSettings (..), HasOidcEnvironment (..), MonadDB (..), getConfiguration, getPool)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
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

import Crypto.JWT (HasClaimsSet(..), ClaimsSet, emptyClaimsSet, NumericDate (..), Audience (..), stringOrUri, StringOrURI(..) )
import Control.Lens
import Network.URI (parseURI, parseURIReference, uriPath, relativeTo, uriToString)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)
import Configuration (getRootURI)


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

sessionClaims :: URI -> Tokens OtherClaims -> Maybe SessionClaims
sessionClaims uri tokens = do
  let claims = otherClaims $ idToken tokens
  issuer <- parseURI $ Data.Text.unpack $ iss $ idToken tokens
  subscriber <- parseURIReference $ Data.Text.unpack $ sub $ idToken tokens
  let subAndIss = subscriber `relativeTo` issuer
  subAndIss' <- preview stringOrUri $ uriToString id subAndIss ""
  audience <- preview stringOrUri $ uriToString id uri ""
  pure $ emptyClaimsSet
     & claimSub ?~ subAndIss'
     & claimAud ?~ Audience [audience]
     & SessionClaims
