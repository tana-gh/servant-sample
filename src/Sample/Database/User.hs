{-# LANGUAGE ScopedTypeVariables #-}

module Sample.Database.User where

import Conduit
import Data.Either.Combinators
import Database.Esqueleto as E
import Sample.Migrations
import Sample.Password

insertUser :: String -> String -> Maybe Int -> SqlPersistM (Maybe (Entity User))
insertUser name password age = do
    hash   <- liftIO $ fromRight' <$> generatePasswordHash password
    userId <- insert $ User name hash age
    result <- select . from $ \user -> do
        where_ $ user ^. UserId E.==. val userId
        return user
    case result of
        user : _ ->
            return $ Just user
        _　->
            return Nothing

selectAllUsers :: SqlPersistM [Entity User]
selectAllUsers =
    select . from $ \(user :: SqlExpr (Entity User)) -> return user

selectUser :: String -> SqlPersistM (Maybe (Entity User))
selectUser name = do
    result <- select . from $ \user -> do
        where_ $ user ^. UserName E.==. val name
        return user
    case result of
        user : _ ->
            return $ Just user
        _　->
            return Nothing
