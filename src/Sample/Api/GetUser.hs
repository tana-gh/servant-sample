module Sample.Api.GetUser where

import Database.Persist
import Sample.Api.Types
import Sample.Database
import Sample.Migrations
import Servant

getUser :: Entity User -> String -> MyHandler (Entity User)
getUser _ name = do
    mUser <- runSql $ selectUser name
    case mUser of
        Just u  -> return u
        Nothing -> throwError err404
