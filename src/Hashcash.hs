{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hashcash (Hashcash) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import JsonWorkProof
import Network.Wai (rawPathInfo, requestHeaders)
import Servant
import Servant.Server.Internal

-- I am very thankful for
-- https://www.williamyaoh.com/posts/2023-02-28-writing-servant-combinators.html
-- which explained how to build such server combinators

data Hashcash
  deriving (Typeable)

instance (HasServer api context) => HasServer (Hashcash :> api) context where
  type ServerT (Hashcash :> api) m = ServerT api m

  hoistServerWithContext ::
    Proxy (Hashcash :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (Hashcash :> api) m ->
    ServerT (Hashcash :> api) n
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

  route ::
    Proxy (Hashcash :> api) ->
    Context context ->
    Delayed env (Server (Hashcash :> api)) ->
    Router env
  route _ ctx server =
    let serverDummyArg = fmap const server
     in route (Proxy @api) ctx $
          addHeaderCheck
            serverDummyArg
            ( withRequest $ \req -> do
                let maybeToken = lookup "JSON-Work-Proof" (requestHeaders req)
                let eitherJwp = maybe (Left "Missing JSON-Work-Proof header") decodeJWP maybeToken
                case eitherJwp of
                  Left e -> delayedFailFatal $ err402 {errBody = fromStrict . encodeUtf8 . pack $ e}
                  Right jwp -> do
                    let path = rawPathInfo req
                    v <- liftIO $ verify jwp 16 (decodeUtf8 path)
                    case v of
                      Left e -> delayedFailFatal $ err402 {errBody = fromStrict . encodeUtf8 . pack $ e}
                      Right () -> pure ()
            )
