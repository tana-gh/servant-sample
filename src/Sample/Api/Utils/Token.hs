module Sample.Api.Utils.Token where

import Control.Monad.IO.Class
import Database.Persist
import Sample.Api.Types
import Sample.Migrations
import Sample.Token
import Servant
import System.IO

getTokenString' :: Entity User -> MyHandler String
getTokenString' user = do
    eToken <- getTokenString user
    case eToken of
        Left  e -> liftIO (hPutStrLn stderr e) >> throwError err500
        Right x -> return x
