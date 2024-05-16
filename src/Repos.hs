{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Repos
  ( API,
    server,
  )
where

import AppM (AppM, HasConfiguration (..), MonadDB (..), getConfiguration, getPool)
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

ariaControls :: H.AttributeValue -> H.Attribute
ariaControls = customAttribute "aria-controls"
ariaExpanded :: H.AttributeValue -> H.Attribute
ariaExpanded = customAttribute "aria-expanded"
ariaLabel :: H.AttributeValue -> H.Attribute
ariaLabel = customAttribute "aria-label"
ariaCurrent :: H.AttributeValue -> H.Attribute
ariaCurrent = customAttribute "aria-current"
ariaDisabled :: H.AttributeValue -> H.Attribute
ariaDisabled = customAttribute "aria-disabled"

xlinkHref = customAttribute "xlink:href"

type API = "books" :> (Get '[HTML] Homepage) :<|> "sample" :> (Get '[HTML] H.Html)

data Homepage = Homepage

instance ToMarkup Homepage where
  toMarkup Homepage = H.docTypeHtml $ do
    H.head $ do
      H.title "OpenID Connect Servant Example"
      H.style (H.toHtml ("body { font-family: monospace; font-size: 18px; }" :: Text))
    H.body $ do
      H.h1 "OpenID Connect Servant Example"
      H.div $
        H.a ! HA.href "/login" $
          "Click here to login"
      H.ul $ do
        H.li $ do
          H.span "API Key in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('api-key'));" :: Text))
        H.li $ do
          H.span "User ID in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('user-id'));" :: Text))

server ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m
  ) =>
  ServerT API m
server = getBooks :<|> getSample

getBooks :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r) => m Homepage
getBooks = pure Homepage

partialNavbar :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r) => m H.Html
partialNavbar = pure $ do
        H.nav ! HA.class_ "navbar navbar-expand-md navbar-dark fixed-top bg-dark" $ do
            H.div ! HA.class_ "container-fluid" $ do
                H.a ! HA.class_ "navbar-brand" ! HA.href "#" $ "Fixed navbar"
                H.button ! HA.class_ "navbar-toggler" 
                         ! HA.type_ "button" 
                         ! H.dataAttribute "bs-toggle" "collapse" 
                         ! H.dataAttribute "bs-target" "#navbarCollapse" 
                         ! ariaControls "navbarCollapse" 
                         ! ariaExpanded "false" 
                         ! ariaLabel "Toggle navigation" $ do
                    H.span ! HA.class_ "navbar-toggler-icon" $ ""
                H.div ! HA.class_ "collapse navbar-collapse" ! HA.id "navbarCollapse" $ do
                    H.ul ! HA.class_ "navbar-nav me-auto mb-2 mb-md-0" $ do
                        H.li ! HA.class_ "nav-item" $ 
                            H.a ! HA.class_ "nav-link active" 
                                ! ariaCurrent "page" 
                                ! HA.href "#" $ "Home"
                        H.li ! HA.class_ "nav-item" $ 
                            H.a ! HA.class_ "nav-link" 
                                ! HA.href "#" $ "Link"
                        H.li ! HA.class_ "nav-item" $ 
                            H.a ! HA.class_ "nav-link disabled" 
                                ! ariaDisabled "true" $ "Disabled"
                    H.form ! HA.class_ "d-flex" ! HA.role "search" $ do
                        H.input ! HA.class_ "form-control me-2" 
                                ! HA.type_ "search" 
                                ! HA.placeholder "Search" 
                                ! ariaLabel "Search"
                        H.button ! HA.class_ "btn btn-outline-success" 
                                 ! HA.type_ "submit" $ "Search"

partialFooter :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r) => m H.Html
partialFooter = pure $ do
  H.footer ! HA.class_ "py-3 footer mt-auto bg-body-tertiary" $ do
   H.div ! HA.class_ "container" $ do    
    H.div ! HA.class_ "row" $ do
        replicateM_ 3 $ H.div ! HA.class_ "col-6 col-md-2 mb-3" $ do
            H.h5 "Section"
            H.ul ! HA.class_ "nav flex-column" $ do
                H.li ! HA.class_ "nav-item mb-2" $ H.a ! HA.href "#" ! HA.class_ "nav-link p-0 text-body-secondary" $ "Home"
                H.li ! HA.class_ "nav-item mb-2" $ H.a ! HA.href "#" ! HA.class_ "nav-link p-0 text-body-secondary" $ "Features"
                H.li ! HA.class_ "nav-item mb-2" $ H.a ! HA.href "#" ! HA.class_ "nav-link p-0 text-body-secondary" $ "Pricing"
                H.li ! HA.class_ "nav-item mb-2" $ H.a ! HA.href "#" ! HA.class_ "nav-link p-0 text-body-secondary" $ "FAQs"
                H.li ! HA.class_ "nav-item mb-2" $ H.a ! HA.href "#" ! HA.class_ "nav-link p-0 text-body-secondary" $ "About"

        H.div ! HA.class_ "col-md-5 offset-md-1 mb-3" $ do
            H.form $ do
                H.h5 "Subscribe to our newsletter"
                H.p "Monthly digest of what's new and exciting from us."
                H.div ! HA.class_ "d-flex flex-column flex-sm-row w-100 gap-2" $ do
                    H.label ! HA.for "newsletter1" ! HA.class_ "visually-hidden" $ "Email address"
                    H.input ! HA.type_ "text" ! HA.id "newsletter1" ! HA.class_ "form-control" ! HA.placeholder "Email address"
                    H.button ! HA.class_ "btn btn-primary" ! HA.type_ "button" $ "Subscribe"

        H.div ! HA.class_ "d-flex flex-column flex-sm-row justify-content-between py-4 my-4 border-top" $ do
          H.p "© 2024 Company, Inc. All rights reserved."
          H.ul ! HA.class_ "list-unstyled d-flex" $ do
            H.li ! HA.class_ "ms-3" $ H.a ! HA.class_ "link-body-emphasis" ! HA.href "#" $ "A"
            H.li ! HA.class_ "ms-3" $ H.a ! HA.class_ "link-body-emphasis" ! HA.href "#" $ "B"
            H.li ! HA.class_ "ms-3" $ H.a ! HA.class_ "link-body-emphasis" ! HA.href "#" $ "C"

partialPage :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r) => Text -> H.Html -> m H.Html
partialPage title body = do
  config <- asks getConfiguration
  let jsPath = H.toValue $ getJavascriptPath config
  let cssPath = H.toValue $ getStylesheetPath config

  navbar <- partialNavbar
  footer <- partialFooter
  
  pure $ H.docTypeHtml $ do
    H.html ! HA.lang "en" ! HA.class_ "h-100" $ do 
      H.head $ do
        H.meta ! HA.charset "utf-8"
        H.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1"
        H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href cssPath
        H.script ! HA.type_ "text/javascript" ! HA.src jsPath $ ""
        H.title $ H.toHtml title
      H.body ! HA.class_ "d-flex flex-column h-100" $ do
        H.header $ do
          navbar
        H.main ! HA.class_ "flex-shrink-0" $ do
          H.div ! HA.class_ "container" $ do
            body
        footer

getSample :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r) => m H.Html
getSample = partialPage "Title" $ do
             H.h1 "Templates!"
             H.p "This will be type-checked, rendered and served"
             H.p "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
             H.p "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
             H.p "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."


