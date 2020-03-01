module Sample.Migrations where

import Relude
import Conduit
import Control.Monad.Logger
import Data.Either.Combinators
import Database.Persist.Sqlite
import Database.Persist.TH
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

doMigration :: Migration -> FilePath -> IO ()
doMigration migration filePath =
    runResourceT . runStderrLoggingT . withSqliteConn (toText filePath) . runReaderT $ do
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
