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
  (
    HasConfiguration (..),
    HasUser (..),
    MonadDB (..),
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
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html5 ((!))
import Text.HTML.TagSoup
import Views.Page (partialPage)
import Configuration
import User


-- Extract text from the first <h1> tag using partitions
extractH1 :: Text -> Maybe Text
extractH1 html = case partitions (isTagOpenName "h1") (parseTags html) of
  [] -> Nothing
  (x : _) -> Just (innerText $ takeWhile (not . isTagCloseName "h1") x)

type API = (Get '[HTML] H.Html) :<|>
  ("about" :>
   (("readme" :> (Get '[HTML] H.Html)) :<|>
    ("api" :> (Get '[HTML] H.Html)))) :<|>
  ("help" :>
   (("instructors" :> (Get '[HTML] H.Html)) :<|>
    ("authors" :> (Get '[HTML] H.Html)) :<|>
    ("students" :> (Get '[HTML] H.Html))))

landingPage :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasUser r) => m H.Html
landingPage = do
  websiteName <- asks (getWebsiteName . getConfiguration)
 
  user <- asks getUser
  bigButton <- case user of
    AuthenticatedUser _ -> pure $ do
      H.a ! HA.href "/login" ! HA.class_ "btn btn-primary btn-lg px-4 gap-3" $ "Login"
    Unauthenticated -> pure $ do
      H.a ! HA.href "/login" ! HA.class_ "btn btn-primary btn-lg px-4 gap-3" $ "Login"

  partialPage "/" $ do
    H.div ! HA.class_ "px-4 py-5 my-5 text-center" $ do
      H.h1 ! HA.class_ "display-5 fw-bold text-body-emphasis" $ H.toHtml websiteName
      H.div ! HA.class_ "col-lg-6 mx-auto" $ do
        H.p ! HA.class_ "lead mb-4" $ do
          "This is your "
          H.strong "decentralized gradebook"
          ": instructors and students can log in to track their progress across other websites.  Your website can use our API to read and write student scores without the complexity of LTI."
        H.p ! HA.class_ "lead mb-4" $ do
          "It's like adding "
          H.preEscapedString "&ldquo;"
          "Name: "
          H.span ! HA.style "text-decoration: underline;" $ H.preEscapedString "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
          H.preEscapedString "&rdquo;"
          " to every website on the Internet."
        H.div ! HA.class_ "d-grid gap-2 d-sm-flex justify-content-sm-center" $ do
          bigButton
          H.a ! HA.href "/readme" ! HA.class_ "btn btn-outline-secondary btn-lg px-4" $ "Learn More"

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
server root = landingPage
                :<|> (render (root </> "README.md") :<|> render (root </> "API.md"))
                :<|> ((render (root </> "getting-started/for-instructors.md")) :<|>
                      (render (root </> "getting-started/for-authors.md")) :<|>
                      (render (root </> "getting-started/for-students.md")))
