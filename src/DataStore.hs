{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataStore (runSql, selectAllUsers, selectOneUser) where

import Conduit
import Config
import Control.Monad.Logger
import Control.Monad.Reader.Class
import Database.Esqueleto as E
import Migration

runSql :: (MonadReader MyConfig m, MonadIO m) => SqlPersistM a -> m a
runSql sql = do
    pool <- asks myConfigPool
    liftIO . runResourceT . runNoLoggingT $ (`runSqlPool` pool) sql

selectAllUsers :: SqlPersistM [Entity User]
selectAllUsers =
    select . from $ \(user :: SqlExpr (Entity User)) -> return user

selectOneUser :: String -> SqlPersistM (Maybe (Entity User))
selectOneUser name = do
    result <- select . from $
        \user -> do
            where_ $ user ^. UserName E.==. val name
            return user
    case result of
        user : _
            -> return $ Just user
        _
            -> return Nothing
