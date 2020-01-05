{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad.Logger
import Data.Pool
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp
import Sample.App
import Sample.Config
import Sample.Key
import Sample.Migrations
import Servant.Auth.Server

main :: IO ()
main = do
    let cookies = defaultCookieSettings
    jwts <- defaultJWTSettings <$> readKey getKeyFilePath 
    bracket
        (runStderrLoggingT $ createSqlitePool getSqliteFilePath 8)
        destroyAllResources
        ( \pool -> do
            let config = MyConfig cookies jwts pool
            putStrLn "Listening on port 8080"
            run 8080 $ app config
        )
