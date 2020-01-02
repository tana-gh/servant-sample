{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (MyApi, server) where

import Config
import Control.Monad.Reader
import Database.Persist
import DataStore
import Migration
import Servant
import Servant.Auth.Server
import System.IO
import Type

type MyApi = (Auth '[JWT] (Entity User) :> Protected) :<|> Unprotected

type Protected =
    "user" :> "all" :> Get '[JSON] [Entity User] :<|>
    "user" :> Capture "name" String :> Get '[JSON] (Entity User)

type Unprotected =
    "login" :>
    ReqBody '[JSON] LoginParams :>
    Post '[JSON] Token

type MyHandler = ReaderT MyConfig Handler

server :: ServerT MyApi MyHandler
server = protected :<|> unprotected

protected :: AuthResult (Entity User) -> ServerT Protected MyHandler
protected (Authenticated user) =
    getAllUsers user :<|>
    getOneUser  user
protected _ = throwAll err401

unprotected :: ServerT Unprotected MyHandler
unprotected = login

getAllUsers :: Entity User -> MyHandler [Entity User]
getAllUsers _ = runSql selectAllUsers

getOneUser :: Entity User -> String -> MyHandler (Entity User)
getOneUser _ name = do
    mUser <- runSql $ selectOneUser name
    case mUser of
        Just u  -> return u
        Nothing -> throwError err404

login :: LoginParams -> MyHandler Token
login loginParams = do
    mUser <- runSql . selectOneUser $ loginName loginParams
    case mUser of
        Just u  -> Token <$> getJWT u
        Nothing -> throwError err401

getJWT :: Entity User -> MyHandler String
getJWT user = do
    jwts   <- asks myConfigJWTSettings
    eToken <- liftIO $ makeJWT user jwts Nothing
    case eToken of
        Left  e -> liftIO (putError e) >> throwError err500
        Right t -> return $ show t
    where
    putError e = hPutStrLn stderr $ "Fail to generate jwt token: " ++ show e
