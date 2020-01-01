{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataStore (fetchAll, fetchOne) where

import           Conduit
import           Config
import           Control.Monad.Logger
import           Control.Monad.Reader.Class
import qualified Database.Esqueleto as E
import           Database.Persist
import           Database.Persist.Sqlite
import           Migration

runSql :: (MonadReader MyConfig m, MonadIO m) => SqlPersistM a -> m a
runSql sql = do
    pool <- asks myConfigPool
    liftIO . runResourceT . runNoLoggingT .(`runSqlPool` pool) $ do
        runMigration migrateAll
        sql

fetchAll :: (MonadReader MyConfig m, MonadIO m) => m [User]
fetchAll = runSql $ do
    users <- E.select . E.from $ \(user :: E.SqlExpr (Entity User)) -> return user
    return $ fmap entityVal users

fetchOne :: (MonadReader MyConfig m, MonadIO m) => String -> m (Maybe User)
fetchOne name = runSql $ do
    result <- E.select . E.from $
        \user -> do
            E.where_ $ user E.^. UserName E.==. E.val name
            return user
    case result of
        Entity { entityVal = user } : _
            -> return $ Just user
        _
            -> return Nothing
