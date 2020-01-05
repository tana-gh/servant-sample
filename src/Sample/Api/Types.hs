{-# LANGUAGE DeriveGeneric #-}

module Sample.Api.Types where

import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import Sample.Config
import Servant

type MyHandler = ReaderT MyConfig Handler

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
