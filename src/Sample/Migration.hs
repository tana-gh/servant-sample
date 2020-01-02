{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Sample.Migration where

import Conduit
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Either.Combinators
import Data.Text
import Database.Esqueleto
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Sample.Password
import Servant.Auth.Server

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json
        name String
        password String
        age Int Maybe
        deriving Eq Generic Read Show
|]

instance FromJWT (Entity User)
instance ToJWT   (Entity User)

doMigration :: Migration -> IO ()
doMigration migration =
    runResourceT . runStderrLoggingT . withSqliteConn getSqliteFilePath . runReaderT $ do
        runMigration migration
        insertTestData
    where
    insertTestData = do
        insertOneTestData "tana"        "password" (Just 36)
        insertOneTestData "hiki_neet_p" "foobar"   (Just 30)
        return ()
    insertOneTestData name password age = do
        hash <- liftIO $ fromRight' <$> generatePasswordHash password
        _ <- insert $ User name hash age
        return ()

getSqliteFilePath :: Text
getSqliteFilePath = "./sample.sqlite3"
