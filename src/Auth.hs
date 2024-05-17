{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Auth where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    (.:),
  )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as AeT

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics ( Generic )

import Servant
import Servant.Auth.Server as SAS
import Servant.Server

import qualified Web.OIDC.Client as O
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
