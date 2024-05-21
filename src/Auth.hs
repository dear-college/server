{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Auth where

import Control.Lens
import Crypto.JWT
  ( Audience (..),
    ClaimsSet,
    HasClaimsSet (..),
    NumericDate (..),
    StringOrURI (..),
    emptyClaimsSet,
    stringOrUri,
  )
import Data.Aeson
import qualified Data.Aeson.KeyMap as M
import qualified Data.Aeson.Types as AeT
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.URI
  ( parseURI,
    parseURIReference,
    relativeTo,
    uriPath,
    uriToString,
  )
import Servant
import Servant.Auth.Server as SAS
import Servant.Server
import User (Subscriber (..))
import Web.OIDC.Client.Tokens (IdTokenClaims (..), Tokens (..), validateIdToken)
import Web.OIDC.Client.Types (Code, SessionStore (..), State)

stripPathFromURI :: URI -> Maybe Text
stripPathFromURI uri = do
  let noPathUri = uri {uriPath = "", uriQuery = "", uriFragment = ""}
  return . Text.pack $ uriToString id noPathUri ""

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
  pure $
    emptyClaimsSet
      & claimSub ?~ subAndIss'
      & claimAud ?~ Audience [audience]
      & SessionClaims

data ToolClaims = ToolClaims {jwtClaims :: ClaimsSet, scope :: [Text]}
  deriving (Show)

instance HasClaimsSet ToolClaims where
  claimsSet f s = fmap (\a' -> s {jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON ToolClaims where
  parseJSON = withObject "ToolClaims" $ \o ->
    ToolClaims
      <$> parseJSON (Object o)
      <*> o .: "scp"

instance ToJSON ToolClaims where
  toJSON s =
    ins "scp" (scope s) (toJSON (jwtClaims s))
    where
      ins k v (Object o) = Object $ M.insert k (toJSON v) o
      ins _ _ a = a

toolClaims :: URI -> UTCTime -> Subscriber -> URI -> Maybe ToolClaims
toolClaims audience expiry (Subscriber sub) target = do
  target' <- stripPathFromURI target
  audience <- preview stringOrUri $ uriToString id audience ""
  sub' <- preview stringOrUri $ uriToString id sub ""
  let claims =
        emptyClaimsSet
          & claimSub ?~ sub'
          & claimAud ?~ Audience [audience]
          & claimExp ?~ NumericDate expiry
  pure $ ToolClaims claims [target']
