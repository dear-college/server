{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Views.Header
  ( partialHeader )
where

import AppM (AppM, HasConfiguration (..), MonadDB (..), getConfiguration, getPool, HasUser (..))
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

import Views.A11y

import Text.Blaze.Html5 (customAttribute)
import Control.Monad (replicateM_)
import Network.URI (uriToString)

import Views.Branding (applicationName)
import User

xlinkHref = customAttribute "xlink:href"

loginButton :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => m H.Html
loginButton = do
  user <- asks getUser
  case user of
    AuthenticatedUser _ -> pure $ do
                       H.a ! HA.class_ "btn btn-outline-primary"  ! HA.href "/logout" $ "Logout"
    Unauthenticated -> pure $ do
                       H.a ! HA.class_ "btn btn-primary"  ! HA.href "/login" $ "Login"

partialHeader :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasUser r, HasConfiguration r) => m H.Html
partialHeader = do
  config <- asks getConfiguration
  let root :: URI = getRootURI config
  let root' = uriToString id root ""

  button <- loginButton
  
  pure $ do
     H.header $ do
        H.nav ! HA.class_ "navbar navbar-expand-md navbar-dark fixed-top bg-dark" $ do
            H.div ! HA.class_ "container-fluid" $ do
                H.a ! HA.class_ "navbar-brand" ! HA.href (H.stringValue root') $ H.toHtml applicationName
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
                    H.div ! HA.class_ "d-flex" $ do
                      button

