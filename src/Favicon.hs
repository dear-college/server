{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Favicon
  ( API,
    server,
    header
  )
where

import AppM
  ( HasConfiguration (..),
    HasUser (..),
    MonadDB (..),
  )
import CMark (commonmarkToHtml)
import Configuration
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Servant
import Servant.HTML.Blaze
import System.FilePath ((</>))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.HTML.TagSoup
import Network.Wai.Application.Static
import WaiAppStatic.Types (unsafeToPiece)

-- <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
-- <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
-- <link rel="manifest" href="/site.webmanifest">

header :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasUser r, HasConfiguration r) => m H.Html
header = pure $ do
  H.link ! HA.rel "apple-touch-icon" ! HA.sizes "180x180" ! HA.href "/apple-touch-icon.png"
  H.link ! HA.rel "icon" ! HA.type_ "image/png" ! HA.sizes "32x32" ! HA.href "/favicon-32x32.png"
  H.link ! HA.rel "icon" ! HA.type_ "image/png" ! HA.sizes "16x16" ! HA.href "/favicon-16x16.png"
  H.link ! HA.rel "manifest" ! HA.href "/site.webmanifest"

type API = "android-chrome-192x192.png" :> Raw :<|>
           "android-chrome-512x512.png" :> Raw :<|>
           "apple-touch-icon.png" :> Raw :<|>
           "favicon-16x16.png" :> Raw :<|>
           "favicon-32x32.png" :> Raw :<|>
           "favicon.ico" :> Raw :<|>
           "site.webmanifest" :> Raw 

fileServerSettings :: FilePath -> Text -> StaticSettings
fileServerSettings dir filename = (defaultFileServerSettings dir)
    { ssLookupFile = \pieces ->
        ssLookupFile (defaultFileServerSettings dir) [unsafeToPiece filename]
    }

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
server root = serve "android-chrome-192x192.png" :<|>
              serve "android-chrome-512x512.png" :<|>
              serve "apple-touch-icon.png" :<|>
              serve "favicon-16x16.png" :<|>
              serve "favicon-32x32.png" :<|>
              serve "favicon.ico" :<|>
              serve "site.webmanifest"
  where
    serve file = serveDirectoryWith (fileServerSettings root file)
