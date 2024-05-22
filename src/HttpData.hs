{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module HttpData
where

import Control.Applicative
import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Crypto.JWT
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteArray.Encoding (Base(Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.List (break)
import Data.Maybe
import Data.Pool (withResource)
import qualified Data.Text as Text
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

import GHC.Generics
import Network.URI (URI, parseURI, uriToString)
import Servant
import Servant.Auth.Server.Internal.ConfigTypes
import Servant.HTML.Blaze
import Servant.Server

import Text.Blaze.Html5 (ToMarkup, customAttribute, (!))

import AppM
    (AppM, HasConfiguration(..), HasJwtSettings(..), HasUser(..), MonadDB(..),
     MonadTime, getConfiguration, getJWK, getJwtSettings)
import Auth
import Configuration
import Model
import User
import Views.Page (partialPage)

import qualified Data.ByteString.Char8 as C8

instance FromHttpApiData C8.ByteString where
  parseUrlPiece = Right . C8.pack . Data.Text.unpack
  parseHeader = Right
  parseQueryParam = Right . C8.pack . Data.Text.unpack

instance (HashAlgorithm a) => FromHttpApiData (Digest a) where
  parseUrlPiece piece = do
    (s :: Text) <- parseUrlPiece piece
    let bs :: BS.ByteString = encodeUtf8 s
    case convertFromBase Base16 (bs :: BS.ByteString) of
      Left s -> Left $ Text.pack s
      Right (b :: BS.ByteString) -> case digestFromByteString b of
        Nothing -> Left "Could not convert byte string to digest"
        Just digest -> Right $ digest

instance FromHttpApiData URI where
  parseUrlPiece piece = do
    (s :: Text) <- parseUrlPiece piece
    case parseURI $ Text.unpack s of
      Nothing -> Left "could not parse URI"
      Just s -> Right s

instance AsError Text where
  _Error = prism' embed match
    where
      embed :: Error -> Text
      embed e = Text.pack $ show e
      match :: Text -> Maybe Error
      match _ = Nothing

instance FromHttpApiData SignedJWT where
  parseUrlPiece piece = do
    (s :: Text) <- parseUrlPiece piece
    if "Bearer " `Text.isPrefixOf` s
      then decodeCompact $ LBS.fromStrict $ encodeUtf8 (Text.drop 7 s)
      else Left "should begin with Bearer"
