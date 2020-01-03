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
    "signup" :> ReqBody '[JSON] SignUpParams :> Post '[JSON] Token :<|>
    "logIn" :> ReqBody '[JSON] LogInParams :> Post '[JSON] Token

type MyHandler = ReaderT MyConfig Handler

server :: ServerT MyApi MyHandler
server = protected :<|> unprotected

protected :: AuthResult (Entity User) -> ServerT Protected MyHandler
protected (Authenticated user) =
    getAllUsers user :<|> getOneUser user
protected _ = throwAll err401

unprotected :: ServerT Unprotected MyHandler
unprotected = signUp :<|> logIn

getAllUsers :: Entity User -> MyHandler [Entity User]
getAllUsers _ = runSql selectAllUsers

getOneUser :: Entity User -> String -> MyHandler (Entity User)
getOneUser _ name = do
    mUser <- runSql $ selectUser name
    case mUser of
        Just u  -> return u
        Nothing -> throwError err404

signUp :: SignUpParams -> MyHandler Token
signUp params =
    if signUpPassword params == signUpPasswordConf params
        then do
            mUser <- runSql $ insertUser (signUpName params) (signUpPassword params) (signUpAge params)
            case mUser of
                Just u  -> Token <$> getJWT u
                Nothing -> throwError err400
        else throwError err400

logIn :: LogInParams -> MyHandler Token
logIn params = do
    mUser <- runSql . selectUser $ logInName params
    case mUser of
        Just u ->
            if validatePasswordString (userPassword . entityVal $ u) (logInPassword params)
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
