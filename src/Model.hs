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
    Slug (..),
    worksheetMatchesScope,
    readProgress,
    writeProgress,
    readState,
    writeState,
    addInstructor,
    courseExists,
    coursesForUser,
    isInstructor
  )
where

import AppM (MonadDB (..))
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Crypto.Hash
import Data.ByteArray
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.URI (uriToString)
import Servant
import User

import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import Codec.Compression.GZip (compress)
import Data.ByteString.Lazy as BL
import Codec.Compression.GZip (decompress)
import Control.Exception (catch, SomeException)

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

newtype Slug = Slug Text deriving (Eq, Show, FromHttpApiData)

worksheetMatchesScope :: Worksheet -> Text -> Bool
worksheetMatchesScope _ _ = True

handleResult :: (MonadError ServerError m) => Either a b -> m b
handleResult (Left _) = throwError err500
handleResult (Right v) = pure v

ignoreResult :: (MonadError ServerError m) => Either a b -> m ()
ignoreResult (Left _) = throwError err500
ignoreResult (Right _) = pure ()

readProgress :: (MonadDB m, MonadError ServerError m) => User -> Worksheet -> m Progress
readProgress user@(AuthenticatedUser (Subscriber _)) worksheet = do
  case userId user of
    Nothing -> throwError $ err401
    Just key -> do
      let field :: BS.ByteString = convert $ worksheetHash worksheet
      Progress <$> (handleResult =<< zscore ("progress:" <> key) field)
readProgress _ _ = do
  throwError $ err401

writeProgress :: (MonadDB m, MonadError ServerError m) => User -> Worksheet -> Progress -> m ()
writeProgress user@(AuthenticatedUser (Subscriber _)) worksheet (Progress (Just p)) = do
  case userId user of
    Nothing -> throwError err401
    Just key -> do
      saveWorksheet worksheet
      let field :: BS.ByteString = convert $ worksheetHash worksheet
      result <- zadd ("progress:" <> key) p field
      ignoreResult result
writeProgress (AuthenticatedUser (Subscriber _)) _ (Progress Nothing) = do
  throwError $ err500 {errBody = "Cannot remove progress"}
writeProgress _ _ _ = do
  throwError err401

compressJSON :: Value -> BL.ByteString
compressJSON jsonValue = compress . encode $ jsonValue

decompressJSON :: BL.ByteString -> IO (Either String Value)
decompressJSON compressedData = catch (doDecompressAndParse compressedData) handleErrors
  where
    doDecompressAndParse :: BL.ByteString -> IO (Either String Value)
    doDecompressAndParse data' = do
      let decompressedData = decompress data'
      return $ case eitherDecode decompressedData of
        Left err -> Left $ "JSON parsing error: " ++ err
        Right val -> Right val

    handleErrors :: SomeException -> IO (Either String Value)
    handleErrors e = return $ Left $ "Failed to decompress or parse: " ++ show e

readState :: (MonadDB m, MonadError ServerError m, MonadIO m) => User -> Worksheet -> m Value
readState user@(AuthenticatedUser (Subscriber _)) worksheet = do
  case userId user of
    Nothing -> throwError $ err401
    Just key -> do
      let field :: BS.ByteString = convert $ worksheetHash worksheet
      result <- hget ("state:" <> key) field
      case result of
        Left _ -> throwError $ err500
        Right Nothing -> pure $ object []
        Right (Just p) -> do
          r <- liftIO $ decompressJSON (fromStrict p)
          handleResult r

readState _ _ = do
  throwError $ err401

writeState :: (MonadDB m, MonadError ServerError m) => User -> Worksheet -> Value -> m ()
writeState user@(AuthenticatedUser (Subscriber _)) worksheet v = do
  case userId user of
    Nothing -> throwError err401
    Just key -> do
      saveWorksheet worksheet
      let field :: BS.ByteString = convert $ worksheetHash worksheet
      result <- hset ("state:" <> key) field (toStrict $ compressJSON v)
      ignoreResult result

writeState _ _ _ = do
  throwError err401

saveWorksheet :: (MonadDB m, MonadError ServerError m) => Worksheet -> m ()
saveWorksheet worksheet = do
  let key :: BS.ByteString = "url:" <> (convert $ worksheetHash worksheet)
  let uri = C8.pack $ uriToString id (worksheetUri worksheet) ""
  result <- rset key uri
  ignoreResult result

coursesForUser :: (MonadDB m, MonadError ServerError m) => User -> m [Slug]
coursesForUser user = do
  case userId user of
    Nothing -> throwError err401
    Just key -> do
      result <- smembers ("users:courses:" <> key)
      case result of
        Left _ -> throwError $ err500
        Right cs -> pure $ Slug <$> decodeUtf8 <$> cs

addInstructor :: (MonadDB m, MonadError ServerError m) => User -> Slug -> m ()
addInstructor user (Slug text) = do
  case userId user of
    Nothing -> throwError err401
    Just key -> do
      let slug' :: BS.ByteString = encodeUtf8 text
      _ <- sadd ("courses:instructors:" <> slug') [key]
      _ <- sadd ("instructors:courses:" <> key) [slug']
      _ <- sadd ("users:courses:" <> key) [slug']
      result <- sadd ("courses:users:" <> slug') [key]
      ignoreResult result

countInstructors :: (MonadDB m, MonadError ServerError m) => Slug -> m Integer
countInstructors (Slug text) = do
  let slug' :: BS.ByteString = encodeUtf8 text
  result <- scard ("courses:instructors:" <> slug')
  handleResult result

courseExists :: (MonadDB m, MonadError ServerError m) => Slug -> m Bool
courseExists slug = do
  c <- countInstructors slug
  pure $ c > 0

isInstructor :: (MonadDB m, MonadError ServerError m) => User -> Slug -> m Bool
isInstructor user (Slug text) = do
  case userId user of
    Nothing -> throwError err401
    Just key -> do
      let slug' :: BS.ByteString = encodeUtf8 text
      result <- sismember ("courses:instructors:" <> slug') key
      handleResult result
