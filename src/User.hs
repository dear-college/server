{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module User where

import Network.URI
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU

-- Becaues the subscriber claim in a JWT is a StringOrURI
newtype Subscriber = Subscriber URI deriving (Eq, Show)

data User = AuthenticatedUser Subscriber
          | Unauthenticated

userId :: User -> Maybe BS.ByteString
userId (AuthenticatedUser (Subscriber s)) = do
  Just $ BSU.fromString $ uriToString id s ""
userId _ = Nothing
