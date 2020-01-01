module Config where

import Database.Persist.Sqlite
import Servant.Auth.Server

data MyConfig = MyConfig
    { myConfigCookieSettings :: CookieSettings
    , myConfigJWTSettings    :: JWTSettings
    , myConfigPool           :: ConnectionPool
    }
