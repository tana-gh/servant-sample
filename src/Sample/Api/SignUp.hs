module Sample.Api.SignUp where

import Sample.Api.Types
import Sample.Api.Utils
import Sample.Database
import Servant

signUp :: SignUpParams -> MyHandler Token
signUp params =
    if signUpPassword params == signUpPasswordConf params
        then do
            mUser <- runSql $ insertUser (signUpName params) (signUpPassword params) (signUpAge params)
            case mUser of
                Just u  -> Token <$> getJWT u
                Nothing -> throwError err400
        else throwError err400
