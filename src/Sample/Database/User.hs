module Sample.Database.User where

import Relude
import Database.Esqueleto
import Sample.Migrations
import Sample.Password

insertUser :: String -> String -> Maybe Int -> SqlPersistM (Either String (Entity User))
insertUser name password age = do
    eHash <- liftIO $ generatePasswordHash password
    case eHash of
        Left  e -> return $ Left e
        Right x -> insertUserWithHash x
    where
    insertUserWithHash hash = do
        userId <- insert $ User name hash age
        result <- select . from $ \user -> do
            where_ $ user ^. UserId ==. val userId
            return user
        case result of
            user : _ ->
                return $ Right user
            _　->
                return $ Left "Failed to insert a user."

selectAllUsers :: SqlPersistM [Entity User]
selectAllUsers =
    select . from $ \(user :: SqlExpr (Entity User)) -> return user

selectUser :: String -> SqlPersistM (Maybe (Entity User))
selectUser name = do
    result <- select . from $ \user -> do
        where_ $ user ^. UserName ==. val name
        return user
    case result of
        user : _ ->
            return $ Just user
        _　->
            return Nothing
