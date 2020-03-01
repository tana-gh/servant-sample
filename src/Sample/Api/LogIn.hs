module Sample.Api.LogIn where

import Relude
import Database.Persist
import Sample.Api.Types
import Sample.Api.Utils.Token
import Sample.Database
import Sample.Migrations
import Sample.Password
import Servant

logIn :: LogInParams -> MyHandler Token
logIn params = do
    mUser <- runSql . selectUser $ logInName params
    case mUser of
        Just u ->
            if validatePasswordString (userPassword . entityVal $ u) (logInPassword params)
                then Token <$> getTokenString' u
                else throwError err401
        Nothing -> throwError err401
