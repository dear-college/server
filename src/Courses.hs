{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Courses
  ( API,
    server,
  )
where

import AppM (AppM, HasConfiguration (..), MonadDB (..), getConfiguration, getJWK, getJwtSettings, HasUser(..), HasJwtSettings(..))
import Configuration
import Control.Applicative
import Control.Monad.Except (liftEither, runExceptT, throwError, MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.List (break)
import Data.Maybe
import Data.Pool (withResource)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Redis as R
import Servant
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html5 (ToMarkup, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Text.Blaze.Html5 (customAttribute)
import Control.Monad (replicateM_)
import Network.URI (uriToString)
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.ByteString.Char8 as C8

import Views.Page (partialPage)
import User

import Network.URI (URI, parseURI)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)
import Crypto.JWT
import Control.Lens
import qualified Data.Aeson.KeyMap as M


-- TODO: I am confused about whether URI's are really utf8 or...?
textToURI :: Text -> Maybe URI
textToURI = parseURI . Text.unpack

newtype Slug = Slug Text deriving (Eq, Show, FromHttpApiData)

type API = "courses" :> Capture "slug" Slug :> CaptureAll "path" Text :> (Get '[HTML] H.Html)


data ToolClaims = ToolClaims { jwtClaims :: ClaimsSet, scope :: [Text] }
  deriving (Show)

instance HasClaimsSet ToolClaims where
  claimsSet f s = fmap (\a' -> s { jwtClaims = a' }) (f (jwtClaims s))

instance FromJSON ToolClaims where
  parseJSON = withObject "ToolClaims" $ \o -> ToolClaims
    <$> parseJSON (Object o)
    <*> o .: "scp"

instance ToJSON ToolClaims where
  toJSON s =
    ins "scp" (scope s) (toJSON (jwtClaims s))
    where
      ins k v (Object o) = Object $ M.insert k (toJSON v) o
      ins _ _ a = a

stripPathFromURI :: URI -> Maybe Text
stripPathFromURI uri = do
  let noPathUri = uri { uriPath = "", uriQuery = "", uriFragment = "" }
  return . Text.pack $ uriToString id noPathUri ""

toolClaims :: URI -> UTCTime -> Subscriber -> URI -> Maybe ToolClaims
toolClaims audience expiry (Subscriber sub) target = do
  target' <- stripPathFromURI target
  audience <- preview stringOrUri $ uriToString id audience ""
  sub' <- preview stringOrUri $ uriToString id sub ""
  let claims = emptyClaimsSet
        & claimSub ?~ sub'
        & claimAud ?~ Audience [audience]
        & claimExp ?~ NumericDate expiry
  pure $ ToolClaims claims [target']

server ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadRandom m,
    MonadError ServerError m,
    HasJwtSettings r,
    HasUser r
  ) => 
  ServerT API m
server = redirectToContent

redirectToContent :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m,     HasJwtSettings r,HasConfiguration r, HasUser r) => Slug -> [Text] -> m H.Html
redirectToContent slug pathPieces = do
  let maybeUri = textToURI $ Text.intercalate "/" pathPieces
  case maybeUri of
    Nothing -> throwError $ err404 { errBody = "Invalid URI" }
    Just uri -> do
      user <- asks getUser
      redirectToContentWithUser user slug uri

instance AsError ServerError where
  _Error = prism' embed match
    where embed :: Error -> ServerError
          embed _ = err500
          match :: ServerError -> Maybe Error
          match _ = Nothing

redirectToContentWithUser :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m,     HasJwtSettings r,HasConfiguration r, HasUser r) => User -> Slug -> URI -> m H.Html
redirectToContentWithUser Unauthenticated _ _ = do
  partialPage "not logged in" $ do
             H.h1 "Not logged in"
             H.p "Log in first, then go back and try again."

  
redirectToContentWithUser (AuthenticatedUser user) slug uri = do

  -- TODO: When should the tokens expire?
  issuedAt <- liftIO getCurrentTime
  let expireAt = addUTCTime 86400 issuedAt

  jwtSettings <- asks getJwtSettings
  jwk <- asks getJWK
  
  audience <- asks (getRootURI . getConfiguration)
  let claims = toolClaims audience expireAt user uri
  bestAlg <- bestJWSAlg jwk
  let bestHeader = newJWSHeader ((),bestAlg)
  mJWT <- signJWT jwk bestHeader claims
  let bs = encodeCompact mJWT

  let uri' = uri { uriFragment = "#" <> CL8.unpack bs }
  let uri'' = uriToString id uri' $ ""
  throwError $ err301 { errHeaders = [("Location", C8.pack uri'')] }
  

