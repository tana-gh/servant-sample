module Sample.Api.SignUp where

import Relude
import Sample.Api.Types
import Sample.Api.Utils.Token
import Sample.Database
import Servant

signUp :: SignUpParams -> MyHandler Token
signUp params =
    if signUpPassword params == signUpPasswordConf params
        then do
            mUser <- runSql $ insertUser (signUpName params) (signUpPassword params) (signUpAge params)
            case mUser of
                Left  _ -> throwError err400
                Right u -> Token <$> getTokenString' u
        else throwError err400
