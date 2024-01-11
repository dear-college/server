{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Repos (API
            , server
            ) where

import Servant
import Servant.Server

import Configuration
import qualified Database.Redis as R

import Data.Maybe
import Data.Aeson
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
-- import Data.String.Conversions (cs)
import Data.Pool (withResource)
import Control.Monad.Reader
import Control.Monad.Except

import AppM ( AppM, getConfiguration, getPool, MonadDB(..), HasConfiguration(..) )

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)

import Data.List (break)
import Data.Aeson.Types (Parser)

import Control.Applicative

type API = "books" :> (Get '[JSON] Int)

getBooks :: (MonadError ServerError m) => m Int
getBooks = do
  return 17

server :: (MonadIO m, -- MonadDB m,
           MonadReader r m, HasConfiguration r, MonadError ServerError m) => ServerT API m
server = getBooks


