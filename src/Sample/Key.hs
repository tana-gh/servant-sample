module Sample.Key where

import Servant.Auth.Server

generateKeyFile :: FilePath -> IO ()
generateKeyFile = writeKey
