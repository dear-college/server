{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OIDC.Types (genRandomBS, OIDCEnv (..), OIDCConf (..), initOIDC) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Client
  ( Manager,
    newManager,
  )
import Network.HTTP.Client.TLS
  ( tlsManagerSettings,
  )
import qualified System.Random as Random
import qualified Web.OIDC.Client as O

genRandomBS :: IO BS.ByteString
genRandomBS = do
  g <- Random.newStdGen
  let randomChars = take 42 $ toChar <$> Random.randomRs (0, n) g
  return $ C8.pack $ readable 0 randomChars
  where
    n = length letters - 1
    toChar i = letters !! i
    letters = ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['a' .. 'z']
    readable :: Int -> [Char] -> [Char]
    readable _ [] = []
    readable i str =
      let blocksize = case i of
            0 -> 8
            1 -> 4
            2 -> 4
            3 -> 4
            _ -> 12
          block = take blocksize str
          rest = drop blocksize str
       in if null rest
            then str
            else block ++ "-" ++ readable (i + 1) rest

data OIDCConf = OIDCConf
  { redirectUri :: BS.ByteString,
    clientId :: BS.ByteString,
    clientSecret :: BS.ByteString
  }
  deriving (Show, Eq)

data OIDCEnv = OIDCEnv
  { oidc :: O.OIDC,
    mgr :: Manager,
    genState :: IO BS.ByteString,
    prov :: O.Provider,
    oidcConf :: OIDCConf
  }

initOIDC :: OIDCConf -> IO OIDCEnv
initOIDC c@OIDCConf {..} = do
  mgr <- newManager tlsManagerSettings
  prov <- O.discover "https://accounts.google.com" mgr
  let oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC prov)
  return
    OIDCEnv
      { oidc = oidc,
        mgr = mgr,
        genState = genRandomBS,
        prov = prov,
        oidcConf = c
      }
