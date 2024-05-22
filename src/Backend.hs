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

module Backend
  ( API,
    server,
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
import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Crypto.JWT
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.List (break)
import Data.Maybe
import Data.Pool (withResource)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics
import Model
import Network.URI (URI, parseURI, uriToString)
import Servant
import Servant.Auth.Server.Internal.ConfigTypes
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html5 (ToMarkup, customAttribute, (!))
import User
import Views.Page (partialPage)
import HttpData

type CorsHeaders =
  '[ Header "Access-Control-Allow-Origin" Text,
     Header "Access-Control-Allow-Credentials" Text,
     Header "Access-Control-Allow-Headers" Text
   ]

addCorsHeaders :: Maybe URI -> a -> Headers CorsHeaders a
addCorsHeaders (Just uri) x = do
  let uri' = Text.pack $ uriToString id uri ""
  addHeader uri' $ addHeader "true" $ addHeader "Authorization, X-Worksheet, Content-Type" x
addCorsHeaders Nothing x = do
  addHeader "*" $ addHeader "false" $ addHeader "" x

type ProgressAPI =
  ( (Verb 'OPTIONS 200 '[JSON] (Headers ((Header "Access-Control-Allow-Methods" Text) ': CorsHeaders) NoContent))
      :<|> (Get '[JSON] (Headers CorsHeaders Progress))
      :<|> (ReqBody '[JSON] Progress :> Put '[JSON] (Headers CorsHeaders NoContent))
  )

type API = "api" :> "v1" :> "progress" :> Header "Origin" URI :> Header "Authorization" SignedJWT :> Header "X-Worksheet" Text :> Capture "sha" (Digest SHA256) :> ProgressAPI

server ::
  ( MonadIO m,
    MonadDB m,
    MonadTime m,
    MonadReader r m,
    HasConfiguration r,
    MonadRandom m,
    MonadError ServerError m,
    HasJwtSettings r,
    HasUser r
  ) =>
  ServerT API m
server origin bearer (Just worksheet) sha = do
  let b = hashWith SHA256 $ encodeUtf8 worksheet
  let uri = parseURI $ Text.unpack worksheet

  let ws =
        if (b == sha)
          then case uri of
            Just u -> Just $ Worksheet u worksheet b
            Nothing -> Nothing
          else Nothing

  optionsProgress origin bearer :<|> getProgress ws origin bearer :<|> putProgress ws origin bearer
server origin bearer Nothing sha =
  optionsProgress origin bearer :<|> getProgress Nothing origin bearer :<|> putProgress Nothing origin bearer

optionsProgress ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadRandom m,
    HasJwtSettings r,
    HasUser r,
    MonadError ServerError m
  ) =>
  Maybe URI ->
  Maybe SignedJWT ->
  m (Headers ((Header "Access-Control-Allow-Methods" Text) ': CorsHeaders) NoContent)
optionsProgress (Just origin) _ = do
  pure $ addHeader "GET, PUT" $ addCorsHeaders (Just origin) $ NoContent
optionsProgress Nothing _ = do
  throwError $ err403 {errBody = "CORS requires an Origin header"}

tokenToUser ::
  ( MonadIO m,
    MonadDB m,
    MonadTime m,
    MonadReader r m,
    HasConfiguration r,
    MonadRandom m,
    MonadError ServerError m,
    HasJwtSettings r,
    HasUser r
  ) =>
  SignedJWT ->
  Worksheet ->
  m User
tokenToUser bearer worksheet = do
  jwk <- asks getJWK
  settings <- jwtSettingsToJwtValidationSettings <$> asks getJwtSettings

  r <- runJOSE @JWTError $ do
    (tc :: ToolClaims) <- verifyJWT settings jwk bearer
    pure tc

  case r of
    Left e -> throwError $ err403 {errBody = "Invalid JWT: " <> (TL.encodeUtf8 $ TL.pack $ show e)}
    Right claims -> do
      let sub :: Maybe URI = preview (claimSub . _Just . uri) claims
      let user = case sub of
            Just s -> AuthenticatedUser (Subscriber $ s)
            Nothing -> Unauthenticated
      case listToMaybe $ scope claims of
        Nothing -> throwError $ err403 {errBody = "Invalid JWT: Missing scope"}
        Just (s :: Text) -> do
          if worksheetMatchesScope worksheet s
            then do
              pure user
            else throwError $ err403 {errBody = "Invalid JWT: Scope does not match"}

getProgress ::
  ( MonadIO m,
    MonadDB m,
    MonadTime m,
    MonadReader r m,
    HasConfiguration r,
    MonadRandom m,
    MonadError ServerError m,
    HasJwtSettings r,
    HasUser r
  ) =>
  Maybe Worksheet ->
  Maybe URI ->
  Maybe SignedJWT ->
  m (Headers CorsHeaders Progress)
getProgress (Just worksheet) origin (Just bearer) = do
  user <- tokenToUser bearer worksheet
  p <- readProgress user worksheet
  pure $ addCorsHeaders origin $ p
getProgress _ _ _ = do
  throwError $ err403 {errBody = "Missing fields"}

putProgress ::
  ( MonadIO m,
    MonadDB m,
    MonadTime m,
    MonadReader r m,
    HasConfiguration r,
    MonadRandom m,
    MonadError ServerError m,
    HasJwtSettings r,
    HasUser r
  ) =>
  Maybe Worksheet ->
  Maybe URI ->
  Maybe SignedJWT ->
  Progress ->
  m (Headers CorsHeaders NoContent)
putProgress (Just worksheet) origin (Just bearer) progress = do
  user <- tokenToUser bearer worksheet
  _ <- writeProgress user worksheet progress
  pure $ addCorsHeaders origin $ NoContent
putProgress _ _ _ progress = pure $ addCorsHeaders Nothing $ NoContent
