module Configuration where

import Network.URI (URI (..))

data Configuration = Configuration
  { getRootURI :: URI,
    getJavascriptPath :: FilePath,
    getStylesheetPath :: FilePath
  }
  deriving (Show)

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { getJavascriptPath = "main.js",
      getStylesheetPath = "main.css"
    }

updateRootURI :: Maybe URI -> Configuration -> Configuration
updateRootURI (Just s) config = config {getRootURI = s}
updateRootURI Nothing config = config

updateJavascriptPath :: Maybe FilePath -> Configuration -> Configuration
updateJavascriptPath (Just s) config = config {getJavascriptPath = s}
updateJavascriptPath Nothing config = config

updateStylesheetPath :: Maybe FilePath -> Configuration -> Configuration
updateStylesheetPath (Just s) config = config {getStylesheetPath = s}
updateStylesheetPath Nothing config = config
