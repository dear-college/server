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

import AppM (AppM, HasConfiguration (..), MonadDB (..), getConfiguration, getPool, HasUser(..))
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

import Views.Page (partialPage)
import User

import Network.URI (URI, parseURI)
import Data.Text (Text)

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
    MonadError ServerError m,
    HasUser r
  ) => 
  ServerT API m
server = redirectToContent

redirectToContent :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => Slug -> [Text] -> m H.Html
redirectToContent slug pathPieces = do
  let maybeUri = textToURI $ Text.intercalate "/" pathPieces
  case maybeUri of
    Nothing -> throwError $ err404 { errBody = "Invalid URI" }
    Just uri -> do
      user <- asks getUser
      redirectToContentWithUser user slug uri

redirectToContentWithUser :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => User -> Slug -> URI -> m H.Html
redirectToContentWithUser Unauthenticated _ _ = do
  partialPage "not logged in" $ do
             H.h1 "Not logged in"
             H.p "Log in first, then go back and try again."
  
redirectToContentWithUser _ slug uri = do
  partialPage "Courses" $ do
             H.h1 "Courses"
             H.p $ H.toHtml $ show $ slug
             H.p $ H.toHtml $ show $ uri
             H.p "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

