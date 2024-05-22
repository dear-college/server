module Configuration where

import Network.URI (URI (..), uriRegName)

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

getWebsiteName :: Configuration -> String
getWebsiteName config =
  let root = getRootURI config in
  case uriAuthority root of
    Just auth -> uriRegName auth
    Nothing -> show root

updateRootURI :: Maybe URI -> Configuration -> Configuration
updateRootURI (Just s) config = config {getRootURI = s}
updateRootURI Nothing config = config

updateJavascriptPath :: Maybe FilePath -> Configuration -> Configuration
updateJavascriptPath (Just s) config = config {getJavascriptPath = s}
updateJavascriptPath Nothing config = config

updateStylesheetPath :: Maybe FilePath -> Configuration -> Configuration
updateStylesheetPath (Just s) config = config {getStylesheetPath = s}
updateStylesheetPath Nothing config = config
