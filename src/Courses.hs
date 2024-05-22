{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Courses
  ( API,
    server,
  )
where

import AppM
  ( AppM,
    HasConfiguration (..),
    HasJwtSettings (..),
    HasUser (..),
    MonadDB (..),
    getConfiguration,
    getJWK,
    getJwtSettings,
  )
import Auth
import Configuration
import Control.Applicative
import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Crypto.JWT
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.List (break)
import Data.Maybe
import Data.Pool (withResource)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Network.URI (URI, parseURI, uriToString)
import Servant
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html5 (ToMarkup, customAttribute, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import User
import Views.Page (partialPage)

-- TODO: I am confused about whether URI's are really utf8 or...?
textToURI :: Text -> Maybe URI
textToURI = parseURI . Text.unpack

newtype Slug = Slug Text deriving (Eq, Show, FromHttpApiData)

type API = "courses" :> Capture "slug" Slug :> CaptureAll "path" Text :> (Get '[HTML] H.Html)

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

redirectToContent :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => Slug -> [Text] -> m H.Html
redirectToContent slug pathPieces = do
  let maybeUri = textToURI $ Text.intercalate "/" pathPieces
  case maybeUri of
    Nothing -> throwError $ err404 {errBody = "Invalid URI"}
    Just uri -> do
      user <- asks getUser
      redirectToContentWithUser user slug uri

instance AsError ServerError where
  _Error = prism' embed match
    where
      embed :: Error -> ServerError
      embed _ = err500
      match :: ServerError -> Maybe Error
      match _ = Nothing

redirectToContentWithUser :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => User -> Slug -> URI -> m H.Html
redirectToContentWithUser Unauthenticated (Slug text) uri = do
  partialPage "join" $ do
    H.div ! HA.class_ "px-4 py-5 my-5 text-center" $ do
      H.h1 ! HA.class_ "display-5 fw-bold text-body-emphasis" $ do
        "Join "
        H.preEscapedString "&ldquo;"
        H.toHtml text
        H.preEscapedString "&rdquo;"      
      H.div ! HA.class_ "col-lg-6 mx-auto" $ do
        H.p ! HA.class_ "lead mb-4" $ do
          "You have not yet logged in.  Please log in and then you will be redirected to "
          H.code (H.toHtml $ show uri)
        H.div ! HA.class_ "d-grid gap-2 d-sm-flex justify-content-sm-center" $ do
          H.a ! HA.href "/login" ! HA.class_ "btn btn-primary btn-lg px-4 gap-3" $ "Login"          

redirectToContentWithUser (AuthenticatedUser user) slug uri = do
  -- TODO: When should the tokens expire?
  issuedAt <- liftIO getCurrentTime
  let expireAt = addUTCTime 86400 issuedAt
  liftIO $ print $ "creaing JWT expiring at " <> (show expireAt)

  jwtSettings <- asks getJwtSettings
  jwk <- asks getJWK

  audience <- asks (getRootURI . getConfiguration)
  let claims = toolClaims audience expireAt user uri
  bestAlg <- bestJWSAlg jwk
  let bestHeader = newJWSHeader ((), bestAlg)
  mJWT <- signJWT jwk bestHeader claims
  let bs = encodeCompact mJWT

  let uri' = uri {uriFragment = "#" <> CL8.unpack bs}
  let uri'' = uriToString id uri' $ ""
  throwError $ err302 {errHeaders = [("Location", C8.pack uri'')]}
