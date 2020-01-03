{-# LANGUAGE DeriveGeneric #-}

module Sample.Types where

import Data.Aeson
import GHC.Generics

data SignUpParams = SignUpParams
    { signUpName         :: String
    , signUpPassword     :: String
    , signUpPasswordConf :: String
    , signUpAge          :: Maybe Int
    } deriving (Eq, Generic, Read, Show)

instance FromJSON SignUpParams
instance ToJSON   SignUpParams

data LogInParams = LogInParams
    { logInName     :: String
    , logInPassword :: String
    } deriving (Eq, Generic, Read, Show)

instance FromJSON LogInParams
instance ToJSON   LogInParams

newtype Token = Token
    { tokenString :: String
    } deriving (Eq, Generic, Read, Show)

instance FromJSON Token
instance ToJSON   Token
