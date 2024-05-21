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

import AppM
  ( AppM,
    HasConfiguration (..),
    HasUser (..),
    MonadDB (..),
    getConfiguration,
    getPool,
  )
import CMark (commonmarkToHtml)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Servant
import Servant.HTML.Blaze
import System.FilePath ((</>))
import qualified Text.Blaze.Html5 as H
import Text.HTML.TagSoup
import Views.Page (partialPage)

-- Extract text from the first <h1> tag using partitions
extractH1 :: Text -> Maybe Text
extractH1 html = case partitions (isTagOpenName "h1") (parseTags html) of
  [] -> Nothing
  (x : _) -> Just (innerText $ takeWhile (not . isTagCloseName "h1") x)

type API = (Get '[HTML] H.Html) :<|> "readme" :> (Get '[HTML] H.Html)

landingPage :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => m H.Html
landingPage = do
  partialPage "/" $ do
    H.h1 "Hello!"
   
render :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => FilePath -> m H.Html
render filename = do
  content <- liftIO $ TIO.readFile filename
  let content' = commonmarkToHtml [] content

  let title = fromMaybe "page" $ extractH1 content'
  partialPage title $ do
    H.preEscapedToHtml content'

server ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m,
    HasUser r
  ) =>
  FilePath ->
  ServerT API m
server root = landingPage :<|> render (root </> "README.md")
