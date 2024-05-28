{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import App (theApplicationWithSettings)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (when)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  fileExists <- doesFileExist ".env"
  when fileExists $ Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  port <- lookupEnv "PORT"
  let settings = maybe id (setPort . read) port defaultSettings

  withStdoutLogger $ \aplogger -> do
    let settingsWithLog = setLogger aplogger settings
    theApp <- theApplicationWithSettings settings
    runSettings settingsWithLog theApp
