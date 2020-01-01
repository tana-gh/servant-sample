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
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Servant.Auth.Server

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json
        name String
        age Int Maybe
        deriving Eq Generic Read Show
    BlogPost json
        title String
        content String
        authorId UserId
        deriving Eq Generic Read Show
|]

instance FromJWT User
instance ToJWT   User

doMigration :: Migration -> IO ()
doMigration migration =
    runResourceT . runStderrLoggingT . withSqliteConn "./sample.db" . runReaderT $ do
        runMigration migration
        tanaId <- insert $ User "tana" $ Just 36
        _      <- insert $ BlogPost "blog1" "content1" tanaId
        hikiId <- insert $ User "hiki_neet_p" $ Just 36
        _      <- insert $ BlogPost "blog2" "content2" hikiId
        return ()
