module Sample.Key where

import Relude
import Servant.Auth.Server

generateKeyFile :: FilePath -> IO ()
generateKeyFile = writeKey
