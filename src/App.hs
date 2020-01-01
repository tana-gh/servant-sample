{-# LANGUAGE DataKinds #-}

module App (app) where

import Api
import Config
import Control.Monad.Reader
import Servant
import Servant.Auth.Server

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
