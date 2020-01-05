module Sample.Api.Utils where

import Control.Monad.Reader
import Database.Persist
import Sample.Api.Types
import Sample.Config
import Sample.Migrations
import Servant
import Servant.Auth.Server
import System.IO

getJWT :: Entity User -> MyHandler String
getJWT user = do
    jwts   <- asks myConfigJWTSettings
    eToken <- liftIO $ makeJWT user jwts Nothing
    case eToken of
        Left  e -> liftIO (putError e) >> throwError err500
        Right t -> return $ show t
    where
    putError e = hPutStrLn stderr $ "Fail to generate jwt token: " ++ show e
