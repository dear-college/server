{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Views.Page
  ( partialPage )
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

import Views.Header (partialHeader)
import Views.Footer (partialFooter)
import Views.Branding

partialPage :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => Text -> H.Html -> m H.Html
partialPage title body = do
  config <- asks getConfiguration
  let jsPath = H.toValue $ getJavascriptPath config
  let cssPath = H.toValue $ getStylesheetPath config

  header <- partialHeader
  footer <- partialFooter
  
  pure $ H.docTypeHtml $ do
    H.html ! HA.lang "en" ! HA.class_ "h-100" $ do 
      H.head $ do
        H.meta ! HA.charset "utf-8"
        H.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1"
        H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href cssPath
        H.script ! HA.type_ "text/javascript" ! HA.src jsPath $ ""
        H.title $ H.toHtml (title <> " - " <> applicationName)
      H.body ! HA.class_ "d-flex flex-column h-100" $ do
        H.header $ do
          header
        H.main ! HA.class_ "flex-shrink-0" $ do
          H.div ! HA.class_ "container" $ do
            body
        footer
