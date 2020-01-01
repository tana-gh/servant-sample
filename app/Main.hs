{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import Config
import Control.Monad.Logger
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp
import Servant.Auth.Server

main :: IO ()
main = do
    let cookies = defaultCookieSettings
    jwts <- defaultJWTSettings <$> generateKey
    pool <- runStderrLoggingT $ createSqlitePool "./sample.db" 8
    let config = MyConfig cookies jwts pool
    putStrLn "Listening on port 8080"
    run 8080 $ app config
