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

module Markdown
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
import CMark (commonmarkToHtml)
import qualified Data.Text.IO as TIO


type API = "readme" :> (Get '[HTML] H.Html)

render :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => FilePath -> m H.Html
render filename = do
  content <- liftIO $ TIO.readFile filename
  let content' = commonmarkToHtml [] content
  
  partialPage "Title" $ do
    H.preEscapedToHtml content'

server ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m,
    HasUser r
  ) => 
  ServerT API m
server = render "README.md"
