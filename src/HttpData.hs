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

import Control.Lens
import Crypto.JWT
import Data.ByteArray.Encoding (Base(Base16), convertFromBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)

import Network.URI (parseURI)
import Servant

import qualified Data.ByteString.Char8 as C8

instance FromHttpApiData C8.ByteString where
  parseUrlPiece = Right . C8.pack . Data.Text.unpack
  parseHeader = Right
  parseQueryParam = Right . C8.pack . Data.Text.unpack

instance (HashAlgorithm a) => FromHttpApiData (Digest a) where
  parseUrlPiece piece = do
    let bs = encodeUtf8 piece
    case convertFromBase Base16 bs of
      Left err -> Left $ Text.pack err
      Right (b :: BS.ByteString)  -> maybe (Left "Could not convert byte string to digest") Right (digestFromByteString b)

instance FromHttpApiData URI where
  parseUrlPiece piece =
    maybe (Left "Could not parse URI") Right (parseURI $ unpack piece)


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
