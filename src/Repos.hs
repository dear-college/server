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

-- import Data.String.Conversions (cs)

import AppM (AppM, HasConfiguration (..), MonadDB (..), getConfiguration, getPool)
import Configuration
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Except (liftEither, runExceptT, throwError)
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
import Servant.Server

type API = "books" :> (Get '[JSON] Int)

getBooks :: (MonadError ServerError m) => m Int
getBooks = do
  return 17

server ::
  ( MonadIO m, -- MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m
  ) =>
  ServerT API m
server = getBooks
