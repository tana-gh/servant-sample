{-# LANGUAGE FlexibleContexts #-}

module Sample.Database
    ( insertUser
    , selectAllUsers
    , selectUser
    , runSql
    ) where

import Conduit
import Control.Monad.Logger
import Control.Monad.Reader.Class
import Database.Esqueleto
import Sample.Config
import Sample.Database.User

runSql :: (MonadReader MyConfig m, MonadIO m) => SqlPersistM a -> m a
runSql sql = do
    pool <- asks myConfigPool
    liftIO . runResourceT . runNoLoggingT $ (`runSqlPool` pool) sql
