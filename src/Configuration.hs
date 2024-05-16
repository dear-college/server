module Configuration where

import qualified Data.ByteString as BS
import Network.URI (URI (..))

data Configuration = Configuration
  { githubRoot :: String,
    githubAccessToken :: String,
    getRootURI :: URI,
    getJavascriptPath :: FilePath,
    getStylesheetPath :: FilePath
  }
  deriving (Show)

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { githubRoot = "http://localhost:4000/github/",
      githubAccessToken = "",
      getJavascriptPath = "main.js",
      getStylesheetPath = "main.css"
    }

updateGithubRoot :: Maybe String -> Configuration -> Configuration
updateGithubRoot (Just s) config = config {githubRoot = s}
updateGithubRoot Nothing config = config

updateGithubAccessToken :: Maybe String -> Configuration -> Configuration
updateGithubAccessToken (Just s) config = config {githubAccessToken = s}
updateGithubAccessToken Nothing config = config

updateRootURI :: Maybe URI -> Configuration -> Configuration
updateRootURI (Just s) config = config {getRootURI = s}
updateRootURI Nothing config = config

updateJavascriptPath :: Maybe FilePath -> Configuration -> Configuration
updateJavascriptPath (Just s) config = config {getJavascriptPath = s}
updateJavascriptPath Nothing config = config

updateStylesheetPath :: Maybe FilePath -> Configuration -> Configuration
updateStylesheetPath (Just s) config = config {getStylesheetPath = s}
updateStylesheetPath Nothing config = config
