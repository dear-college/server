{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Views.Footer
  ( partialFooter,
  )
where

import AppM (AppM, HasConfiguration (..), MonadDB (..), getConfiguration, getPool)
import Configuration
import Control.Applicative
import Control.Monad (replicateM_)
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
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
import Data.Time
import Data.Time.Calendar
import qualified Database.Redis as R
import Network.URI (uriToString)
import Servant
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html5 (ToMarkup, customAttribute, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Views.A11y
import Views.Branding

getCurrentYear :: IO Integer
getCurrentYear = do
  (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
  return year

partialFooter :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r) => m H.Html
partialFooter = do
  year <- liftIO getCurrentYear
  pure $ do
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

          H.div ! HA.class_ "d-flex flex-column flex-sm-row justify-content-between pt-4 mt-2 border-top" $ do
            H.p $ do
              H.html "© "
              H.html $ H.toHtml year
              H.html " "
              H.a ! HA.href "https://kisonecat.com/" $ "Jim Fowler"
              H.html "; licensed under the "
              H.a ! HA.href "https://www.gnu.org/licenses/agpl-3.0.txt" $ "GNU Affero General Public License v3.0 or later"
              H.html "."
          H.div ! HA.class_ "d-flex flex-column flex-sm-row justify-content-between" $ do
            H.p ! HA.class_ "text-muted" $ do
              H.html "This website is based upon work supported by the National Science Foundation under NSF "
              H.a ! HA.href "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1915363" $ "DUE–1915363"
              H.html ". "
              H.html "Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation."
