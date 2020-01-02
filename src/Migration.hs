{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Migration where

import Conduit
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text
import Database.Esqueleto
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Servant.Auth.Server

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json
        name String
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
        _ <- insert $ User "tana"        $ Just 36
        _ <- insert $ User "hiki_neet_p" $ Just 30
        return ()

getSqliteFilePath :: Text
getSqliteFilePath = "./sample.sqlite3"
