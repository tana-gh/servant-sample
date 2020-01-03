{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sample.DataStore where

import Conduit
import Control.Monad.Logger
import Control.Monad.Reader.Class
import Data.Either.Combinators
import Database.Esqueleto as E
import Sample.Config
import Sample.Migration
import Sample.Password

runSql ::
    (MonadReader MyConfig m, MonadIO m) =>
    SqlPersistM a -> m a
runSql sql = do
    pool <- asks myConfigPool
    liftIO . runResourceT . runNoLoggingT $ (`runSqlPool` pool) sql

insertUser :: String -> String -> Maybe Int -> SqlPersistM (Maybe (Entity User))
insertUser name password age = do
    hash   <- liftIO $ fromRight' <$> generatePasswordHash password
    userId <- insert $ User name hash age
    result <- select . from $ \user -> do
        where_ $ user ^. UserId E.==. val userId
        return user
    case result of
        user : _ ->
            return $ Just user
        _　->
            return Nothing

selectAllUsers :: SqlPersistM [Entity User]
selectAllUsers =
    select . from $ \(user :: SqlExpr (Entity User)) -> return user

selectUser :: String -> SqlPersistM (Maybe (Entity User))
selectUser name = do
    result <- select . from $ \user -> do
        where_ $ user ^. UserName E.==. val name
        return user
    case result of
        user : _ ->
            return $ Just user
        _　->
            return Nothing
