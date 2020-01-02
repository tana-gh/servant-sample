module Key where

import Servant.Auth.Server

generateKeyFile :: IO ()
generateKeyFile =
    writeKey getKeyFilePath

getKeyFilePath :: FilePath
getKeyFilePath = "./.key"
