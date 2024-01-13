{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import App (theApplicationWithSettings)
import Configuration.Dotenv (defaultConfig, loadFile)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    getPort,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
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
