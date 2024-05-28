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
  ( HasConfiguration (..),
    HasJwtSettings (..),
    HasUser (..),
    MonadDB (..),
    MonadTime,
    getJWK,
    getJwtSettings,
  )
import Auth
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Crypto.JWT
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Hashcash
import HttpData ()
import Model
import Network.URI (parseURI, uriToString)
import Servant
import Servant.Auth.Server.Internal.ConfigTypes
import User

type CorsHeaders =
  '[ Header "Access-Control-Allow-Origin" Text,
     Header "Access-Control-Allow-Credentials" Text,
     Header "Access-Control-Allow-Headers" Text
   ]

addCorsHeaders :: Maybe URI -> a -> Headers CorsHeaders a
addCorsHeaders (Just url) x = do
  let url' = Text.pack $ uriToString id url ""
  addHeader url' $ addHeader "true" $ addHeader "Authorization, Worksheet, Content-Type, JSON-Work-Proof" x
addCorsHeaders Nothing x = do
  addHeader "*" $ addHeader "false" $ addHeader "" x

type ProgressAPI =
  "progress"
    :> Capture "sha" (Digest SHA256)
    :> ( (Verb 'OPTIONS 200 '[JSON] (Headers ((Header "Access-Control-Allow-Methods" Text) ': CorsHeaders) NoContent))
           :<|> (Get '[JSON] (Headers CorsHeaders Progress))
           :<|> (ReqBody '[JSON] Progress :> Put '[JSON] (Headers CorsHeaders NoContent))
       )

type StateAPI =
  "state"
    :> Capture "sha" (Digest SHA256)
    :> ( (Verb 'OPTIONS 200 '[JSON] (Headers ((Header "Access-Control-Allow-Methods" Text) ': CorsHeaders) NoContent))
           :<|> (Get '[JSON] (Headers CorsHeaders Value))
           :<|> (Hashcash :> ReqBody '[JSON] Value :> Put '[JSON] (Headers CorsHeaders NoContent))
       )

type API =
  "api"
    :> "v1"
    :> Header "Origin" URI
    :> Header "Authorization" SignedJWT
    :> Header "Worksheet" Text
    :> (ProgressAPI :<|> StateAPI)

progressServer ::
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
  Maybe URI ->
  Maybe SignedJWT ->
  Maybe Text ->
  ServerT ProgressAPI m
progressServer origin bearer (Just worksheet) sha = do
  let b = hashWith SHA256 $ encodeUtf8 worksheet
  let url = parseURI $ Text.unpack worksheet

  let ws =
        if (b == sha)
          then case url of
            Just u -> Just $ Worksheet u worksheet b
            Nothing -> Nothing
          else Nothing

  optionsHandler origin bearer :<|> getProgress ws origin bearer :<|> putProgress ws origin bearer
progressServer origin bearer Nothing _ =
  optionsHandler origin bearer :<|> getProgress Nothing origin bearer :<|> putProgress Nothing origin bearer

stateServer ::
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
  Maybe URI ->
  Maybe SignedJWT ->
  Maybe Text ->
  ServerT StateAPI m
stateServer origin bearer (Just worksheet) sha = do
  let b = hashWith SHA256 $ encodeUtf8 worksheet
  let url = parseURI $ Text.unpack worksheet

  let ws =
        if (b == sha)
          then case url of
            Just u -> Just $ Worksheet u worksheet b
            Nothing -> Nothing
          else Nothing

  optionsHandler origin bearer :<|> getState ws origin bearer :<|> putState ws origin bearer
stateServer origin bearer Nothing _ =
  optionsHandler origin bearer :<|> getState Nothing origin bearer :<|> putState Nothing origin bearer

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
server origin bearer worksheet = do
  (progressServer origin bearer worksheet) :<|> (stateServer origin bearer worksheet)

optionsHandler ::
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
optionsHandler (Just origin) _ = do
  pure $ addHeader "GET, PUT" $ addCorsHeaders (Just origin) $ NoContent
optionsHandler Nothing _ = do
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
  key <- asks getJWK
  settings <- jwtSettingsToJwtValidationSettings <$> asks getJwtSettings

  r <- runJOSE @JWTError $ do
    (tc :: ToolClaims) <- verifyJWT settings key bearer
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
putProgress _ _ _ _ = pure $ addCorsHeaders Nothing $ NoContent

getState ::
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
  m (Headers CorsHeaders Value)
getState (Just worksheet) origin (Just bearer) = do
  user <- tokenToUser bearer worksheet
  v <- readState user worksheet
  pure $ addCorsHeaders origin $ v
getState _ _ _ = do
  throwError $ err403 {errBody = "Missing fields"}

putState ::
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
  Value ->
  m (Headers CorsHeaders NoContent)
putState (Just worksheet) origin (Just bearer) state = do
  user <- tokenToUser bearer worksheet
  _ <- writeState user worksheet state
  pure $ addCorsHeaders origin $ NoContent
putState _ _ _ _ = pure $ addCorsHeaders Nothing $ NoContent
