{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Sample.Api (MyApi, server) where

import Database.Persist
import Sample.Api.GetAllUsers
import Sample.Api.GetUser
import Sample.Api.LogIn
import Sample.Api.SignUp
import Sample.Api.Types
import Sample.Migrations
import Servant
import Servant.Auth.Server

type MyApi = (MyAuth :> Protected) :<|> Unprotected

type MyAuth = Auth '[JWT] (Entity User)

type Protected =
    "user" :> "all" :> Get '[JSON] [Entity User] :<|>
    "user" :> Capture "name" String :> Get '[JSON] (Entity User)

type Unprotected =
    "signup" :> ReqBody '[JSON] SignUpParams :> Post '[JSON] Token :<|>
    "login" :> ReqBody '[JSON] LogInParams :> Post '[JSON] Token

server :: ServerT MyApi MyHandler
server = protected :<|> unprotected

protected :: AuthResult (Entity User) -> ServerT Protected MyHandler
protected (Authenticated user) =
    getAllUsers user :<|> getUser user
protected _ = throwAll err401

unprotected :: ServerT Unprotected MyHandler
unprotected = signUp :<|> logIn
