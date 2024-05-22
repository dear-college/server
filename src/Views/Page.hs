{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Views.Page (partialPage) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Data.Text (Text)
import Servant
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as HA

import AppM (AppM, HasConfiguration(..), HasUser(..), MonadDB(..))
import Configuration
import Views.Footer (partialFooter)
import Views.Header (partialHeader)

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
        H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href ("/" <> cssPath)
        H.script ! HA.type_ "text/javascript" ! HA.src ("/" <> jsPath) $ ""
        H.title $ H.toHtml (title <> " - " <> applicationName)
      H.body ! HA.class_ "d-flex flex-column h-100" $ do
        H.header $ do
          header
        H.main ! HA.class_ "flex-shrink-0" $ do
          H.div ! HA.class_ "container" $ do
            body
        footer
