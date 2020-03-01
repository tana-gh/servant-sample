module Sample.Password where

import Relude
import Crypto.BCrypt
import Data.ByteString.Char8

generatePasswordHash :: String -> IO (Either String String)
generatePasswordHash password = do
    mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack password)
    case mHash of
        Just h  -> return . Right $ unpack h
        Nothing -> return $ Left "Failed to generate a password hash."

validatePasswordString :: String -> String -> Bool
validatePasswordString hash attempt = validatePassword (pack hash) (pack attempt)
