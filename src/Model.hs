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

module Model
  ( Progress (..),
    Worksheet (..),
    worksheetMatchesScope,
    readProgress,
    writeProgress,
  )
where

import AppM
  ( AppM,
    HasConfiguration (..),
    HasJwtSettings (..),
    HasUser (..),
    MonadDB (..),
    MonadTime,
    getConfiguration,
    getJWK,
    getJwtSettings,
  )
import Auth
import Configuration
import Control.Applicative
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Crypto.Hash
import Crypto.JWT
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteArray
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.List (break)
import Data.Maybe (fromMaybe)
import Data.Pool (withResource)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics
import Network.URI (URI, parseURI, uriToString)
import Servant
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html5 (ToMarkup, customAttribute, (!))
import User

newtype Progress = Progress (Maybe Double) deriving (Eq, Show)

-- Progress encapsulates a Maybe Double. We convert Just p to a JSON
-- object { "progress": p } and Nothing to an empty JSON object {}.
instance ToJSON Progress where
  toJSON (Progress (Just p)) = object ["progress" .= p]
  toJSON (Progress Nothing) = object []

instance FromJSON Progress where
  parseJSON = withObject "Progress" $ \v -> do
    progress <- v .:? "progress"
    return $ Progress progress

data Worksheet = Worksheet
  { worksheetUri :: URI,
    worksheetText :: Text,
    worksheetHash :: Digest SHA256
  }
  deriving (Show)

worksheetMatchesScope :: Worksheet -> Text -> Bool
worksheetMatchesScope _ _ = True

readProgress :: (MonadDB m, MonadError ServerError m) => User -> Worksheet -> m Progress
readProgress user@(AuthenticatedUser (Subscriber _)) worksheet = do
  case userId user of
    Nothing -> throwError $ err401
    Just key -> do
      let field :: BS.ByteString = convert $ worksheetHash worksheet
      result <- zscore key field
      case result of
        Left _ -> throwError $ err500
        Right p -> pure $ Progress p
readProgress _ _ = do
  throwError $ err401

writeProgress :: (MonadDB m, MonadError ServerError m) => User -> Worksheet -> Progress -> m ()
writeProgress user@(AuthenticatedUser (Subscriber _)) worksheet (Progress (Just p)) = do
  case userId user of
    Nothing -> throwError err401
    Just key -> do
      let field :: BS.ByteString = convert $ worksheetHash worksheet
      result <- zadd key p field
      case result of
        Left _ -> throwError $ err500
        Right _ -> pure ()
writeProgress user@(AuthenticatedUser (Subscriber _)) worksheet (Progress Nothing) = do
  throwError $ err500 {errBody = "Cannot remove progress"}
writeProgress _ _ _ = do
  throwError err401
