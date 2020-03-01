module Sample.Token where

import Relude
import Database.Persist
import Sample.Config
import Sample.Migrations
import Servant.Auth.Server

getTokenString :: (MonadReader MyConfig m, MonadIO m) => Entity User -> m (Either String String)
getTokenString user = do
    jwts   <- asks myConfigJWTSettings
    eToken <- liftIO $ makeJWT user jwts Nothing
    case eToken of
        Left  e -> return . Left  $ "Failed to generate a jwt token: " ++ show e
        Right t -> return . Right $ show t
