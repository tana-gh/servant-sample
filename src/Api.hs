{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (MyApi, server) where

import Config
import Control.Monad.Reader
import DataStore
import Migration
import Servant
import Servant.Auth.Server
import System.IO
import Type

type MyApi = (Auth '[JWT] User :> Protected) :<|> Unprotected

type Protected =
    "user" :> "all" :> Get '[JSON] [User] :<|>
    "user" :> Capture "name" String :> Get '[JSON] User

type Unprotected =
    "login" :>
    ReqBody '[JSON] LoginParams :>
    Post '[JSON] Token

type MyHandler = ReaderT MyConfig Handler

server :: ServerT MyApi MyHandler
server = protected :<|> unprotected

protected :: AuthResult User -> ServerT Protected MyHandler
protected (Authenticated user) =
    getAllUsers user :<|>
    getOneUser  user
protected _ = throwAll err401

unprotected :: ServerT Unprotected MyHandler
unprotected = login

getAllUsers :: User -> MyHandler [User]
getAllUsers _ = fetchAll

getOneUser :: User -> String -> MyHandler User
getOneUser _ name = do
    mUser <- fetchOne name
    case mUser of
        Just u  -> return u
        Nothing -> throwError err404

login :: LoginParams -> MyHandler Token
login loginParams = do
    mUser <- fetchOne $ loginName loginParams
    case mUser of
        Just u  -> Token <$> getJWT u
        Nothing -> throwError err404

getJWT :: User -> MyHandler String
getJWT user = do
    jwts   <- asks myConfigJWTSettings
    eToken <- liftIO $ makeJWT user jwts Nothing
    case eToken of
        Left  e -> liftIO (putError e) >> throwError err500
        Right t -> return $ show t
    where
    putError e = hPutStrLn stderr $ "Fail to generate jwt token: " ++ show e
