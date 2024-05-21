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
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text)
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

import Views.Page (partialPage)
import CMark (commonmarkToHtml)
import qualified Data.Text.IO as TIO

import Text.HTML.TagSoup
import System.FilePath ((</>))

-- Extract text from the first <h1> tag using partitions
extractH1 :: Text -> Maybe Text
extractH1 html = case partitions (isTagOpenName "h1") (parseTags html) of
    [] -> Nothing
    (x:_) -> Just (innerText $ takeWhile (not . isTagCloseName "h1") x)

type API = "readme" :> (Get '[HTML] H.Html)

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
  FilePath -> ServerT API m
server root = render (root </> "README.md")

