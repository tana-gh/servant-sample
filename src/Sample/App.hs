{-# LANGUAGE DataKinds #-}

module Sample.App (app, destroy, initialize, AppEnv (..)) where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Pool
import Data.Text
import Database.Persist.Sqlite
import Sample.Api
import Sample.Config
import Servant
import Servant.Auth.Server
import System.FilePath.Posix

data AppEnv
    = AppEnvProduction
    | AppEnvDevelopment
    | AppEnvTest

initialize :: AppEnv -> IO MyConfig
initialize env = do
    let cookies = defaultCookieSettings
    jwts <- defaultJWTSettings <$> readKey (getKeyFilePath env)
    pool <- runStderrLoggingT $ createSqlitePool (pack $ getSqliteFilePath env) 8
    return $ MyConfig cookies jwts pool

destroy :: MyConfig -> IO ()
destroy config = destroyAllResources $ myConfigPool config

app :: MyConfig -> Application
app config = serveWithContext apiProxy (context config) (toServer config)

toServer :: MyConfig -> ServerT MyApi Handler
toServer config = hoistServerWithContext apiProxy contextProxy (`runReaderT` config) server

context :: MyConfig -> Context '[CookieSettings, JWTSettings]
context config = myConfigCookieSettings config :. myConfigJWTSettings config :. EmptyContext

apiProxy :: Proxy MyApi
apiProxy = Proxy

contextProxy :: Proxy '[CookieSettings, JWTSettings]
contextProxy = Proxy

getKeyFilePath :: AppEnv -> FilePath
getKeyFilePath env = getConfigDirPath env </> ".key"

getSqliteFilePath :: AppEnv -> FilePath
getSqliteFilePath env = getConfigDirPath env </> "sample.sqlite3"

getConfigDirPath :: AppEnv -> FilePath
getConfigDirPath env =
    case env of
        AppEnvProduction  -> "./config/production/"
        AppEnvDevelopment -> "./config/development/"
        AppEnvTest        -> "./config/test/"
