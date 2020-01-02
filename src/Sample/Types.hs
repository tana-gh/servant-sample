{-# LANGUAGE DeriveGeneric #-}

module Sample.Types where

import Data.Aeson
import GHC.Generics

data LoginParams = LoginParams
    { loginName     :: String
    , loginPassword :: String
    } deriving (Eq, Generic, Read, Show)

instance FromJSON LoginParams
instance ToJSON   LoginParams

newtype Token = Token
    { tokenString :: String
    } deriving (Eq, Generic, Read, Show)

instance FromJSON Token
instance ToJSON   Token
