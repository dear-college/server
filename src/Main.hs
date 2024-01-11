{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import App (theApplicationWithSettings)
import Configuration.Dotenv (loadFile, defaultConfig)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp
    (setLogger, setPort, getPort, runSettings, defaultSettings )
import System.Environment (lookupEnv)

main :: IO ()
main = do
  -- with this, lookupEnv will fetch from .env or from an environment variable
  _ <- Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  port <- lookupEnv "PORT"
  let settings = maybe id (setPort . read) port defaultSettings

  withStdoutLogger $ \aplogger -> do
    let settingsWithLog = setLogger aplogger settings
    theApp <- theApplicationWithSettings settings
    runSettings settingsWithLog theApp
