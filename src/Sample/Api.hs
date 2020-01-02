{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Sample.Api (MyApi, server) where

import Control.Monad.Reader
import Database.Persist
import Sample.Config
import Sample.DataStore
import Sample.Migration
import Sample.Password
import Sample.Types
import Servant
import Servant.Auth.Server
import System.IO

type MyApi = (MyAuth :> Protected) :<|> Unprotected

type MyAuth = Auth '[JWT] (Entity User)

type Protected =
    "user" :> "all" :> Get '[JSON] [Entity User] :<|>
    "user" :> Capture "name" String :> Get '[JSON] (Entity User)

type Unprotected =
    "login" :> ReqBody '[JSON] LoginParams :> Post '[JSON] Token

type MyHandler = ReaderT MyConfig Handler

server :: ServerT MyApi MyHandler
server = protected :<|> unprotected

protected :: AuthResult (Entity User) -> ServerT Protected MyHandler
protected (Authenticated user) =
    getAllUsers user :<|> getOneUser user
protected _ = throwAll err401

unprotected :: ServerT Unprotected MyHandler
unprotected = logIn

getAllUsers :: Entity User -> MyHandler [Entity User]
getAllUsers _ = runSql selectAllUsers

getOneUser :: Entity User -> String -> MyHandler (Entity User)
getOneUser _ name = do
    mUser <- runSql $ selectOneUser name
    case mUser of
        Just u  -> return u
        Nothing -> throwError err404

logIn :: LoginParams -> MyHandler Token
logIn loginParams = do
    mUser <- runSql . selectOneUser $ loginName loginParams
    case mUser of
        Just u ->
            if validatePasswordString (userPassword . entityVal $ u) (loginPassword loginParams)
                then Token <$> getJWT u
                else throwError err401
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
