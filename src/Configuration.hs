module Configuration where

import qualified Data.ByteString as BS
import Network.URI (URI (..))

data Configuration = Configuration
  { githubRoot :: String,
    githubAccessToken :: String,
    getRootURI :: URI
  }
  deriving (Show)

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { githubRoot = "http://localhost:4000/github/",
      githubAccessToken = ""
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
