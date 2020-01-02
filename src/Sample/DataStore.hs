{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sample.DataStore where

import Conduit
import Control.Monad.Logger
import Control.Monad.Reader.Class
import Database.Esqueleto as E
import Sample.Config
import Sample.Migration

runSql ::
    (MonadReader MyConfig m, MonadIO m) =>
    SqlPersistM a -> m a
runSql sql = do
    pool <- asks myConfigPool
    liftIO . runResourceT . runNoLoggingT $ (`runSqlPool` pool) sql

selectAllUsers :: SqlPersistM [Entity User]
selectAllUsers =
    select . from $ \(user :: SqlExpr (Entity User)) -> return user

selectOneUser :: String -> SqlPersistM (Maybe (Entity User))
selectOneUser name = do
    result <- select . from $ \user -> do
        where_ $ user ^. UserName E.==. val name
        return user
    case result of
        user : _ ->
            return $ Just user
        _　->
            return Nothing
