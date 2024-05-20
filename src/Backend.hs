{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend
  ( API,
    server,
  )
where

import AppM (AppM, HasConfiguration (..), MonadDB (..), MonadTime, getConfiguration, getJWK, getJwtSettings, HasUser(..), HasJwtSettings(..))
import Configuration
import Control.Applicative
import Control.Monad.Except (liftEither, runExceptT, throwError, MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
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
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html5 (ToMarkup, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Text.Blaze.Html5 (customAttribute)
import Control.Monad (replicateM_)
import Network.URI (uriToString)
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS

import Views.Page (partialPage)
import User

import Network.URI (URI, parseURI)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)
import Crypto.JWT
import Control.Lens
import qualified Data.Aeson.KeyMap as M

import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Data.ByteArray.Encoding (convertFromBase, convertToBase, Base(Base16))
import qualified Data.ByteString.Char8 as C
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Lazy.UTF8 as LazyByteString
import qualified Data.ByteString.UTF8 as ByteString

import Servant.Auth.Server.Internal.ConfigTypes

import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Text.Lazy.IO          as TL

import Control.Monad.Except

import Auth
import Model

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
    where embed :: Error -> Text
          embed e = Text.pack $ show e
          match :: Text -> Maybe Error
          match _ = Nothing

instance FromHttpApiData SignedJWT where
  parseUrlPiece piece = do
    (s :: Text) <- parseUrlPiece piece
    if "Bearer " `Text.isPrefixOf` s
      then decodeCompact $ LBS.fromStrict $ encodeUtf8 (Text.drop 7 s)
      else Left "should begin with Bearer"

type CorsHeaders = '[Header "Access-Control-Allow-Origin" Text,
                     Header "Access-Control-Allow-Credentials" Text,
                     Header "Access-Control-Allow-Headers" Text]
addCorsHeaders :: Maybe URI -> a -> Headers CorsHeaders a
addCorsHeaders (Just uri) x = do
  let uri' = Text.pack $ uriToString id uri ""
  addHeader uri' $ addHeader "true" $ addHeader "Authorization, X-Worksheet" x
addCorsHeaders Nothing x = do
  addHeader "*" $ addHeader "false" $ addHeader "" x

type ProgressAPI =
  (
    (Verb 'OPTIONS 200 '[JSON] (Headers ((Header "Access-Control-Allow-Methods" Text) ': CorsHeaders) NoContent)) :<|>
    (Get '[JSON] (Headers CorsHeaders Progress)) :<|>
    (ReqBody '[JSON] Progress :> Put '[JSON] (Headers CorsHeaders NoContent))
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

  let ws = if (b == sha)
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
    Maybe URI -> Maybe SignedJWT -> m (Headers ((Header "Access-Control-Allow-Methods" Text) ': CorsHeaders) NoContent)
optionsProgress (Just origin) _ = do
  pure $ addHeader "GET, PUT" $ addCorsHeaders (Just origin) $ NoContent
optionsProgress Nothing _ = do
  throwError $ err403 { errBody = "CORS requires an Origin header" }

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
  Maybe Worksheet -> Maybe URI -> Maybe SignedJWT -> m (Headers CorsHeaders Progress)
getProgress (Just worksheet) origin (Just bearer) = do
  jwk <- asks getJWK
  settings <- jwtSettingsToJwtValidationSettings <$> asks getJwtSettings

  r <- runJOSE @JWTError $ do
    -- c <- decodeCompact $ LBS.fromStrict $ encodeUtf8 (Text.drop 7 bearer)
    (tc :: ToolClaims) <- verifyJWT settings jwk bearer
    pure tc

  case r of
    Left e -> throwError $ err403 { errBody = "Invalid JWT: " <> (TL.encodeUtf8 $ TL.pack $ show e) }
    Right claims ->  do
      liftIO $ print claims

      let sub :: Maybe URI = preview (claimSub . _Just . uri) claims
      let user = case sub of
            Just s -> AuthenticatedUser (Subscriber $ s)
            Nothing -> Unauthenticated

      case listToMaybe $ scope claims of
        Nothing -> throwError $ err403 { errBody = "Invalid JWT: Missing scope" }
        Just (s :: Text) -> do
          --liftIO $ print $ s
          --liftIO $ print $ stripPathFromURI worksheet
          if worksheetMatchesScope worksheet s
            then do
              p <- readProgress user worksheet
              pure $ addCorsHeaders origin $ p
            else throwError $ err403 { errBody = "Invalid JWT: Scope does not match" }

getProgress _ _ _ = do
  throwError $ err403 { errBody = "Missing fields" }

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
  Maybe Worksheet -> Maybe URI -> Maybe SignedJWT -> Progress -> m (Headers CorsHeaders NoContent)
putProgress _ _ _ progress = pure $ addCorsHeaders Nothing $ NoContent
