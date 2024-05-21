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

module Repos
  ( API,
    server,
  )
where

import AppM
  ( AppM,
    HasConfiguration (..),
    HasUser (..),
    MonadDB (..),
    getConfiguration,
    getPool,
  )
import Configuration
import Control.Applicative
import Control.Monad (replicateM_)
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 (pack)
import Data.List (break)
import Data.Maybe
import Data.Pool (withResource)
import Data.Text (Text)
import Network.URI (uriToString)
import Servant
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html5 (ToMarkup, customAttribute, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import User
import Views.Page (partialPage)

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
    MonadError ServerError m,
    HasUser r
  ) =>
  ServerT API m
server = getBooks :<|> getSample

getBooks :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r) => m Homepage
getBooks = pure Homepage

getSample :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => m H.Html
getSample = partialPage "Title" $ do
  H.h1 "Templates!"
  H.p "This will be type-checked, rendered and served"
  H.p "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
  H.p "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
  H.p "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
