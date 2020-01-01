{-# LANGUAGE DeriveGeneric #-}

module Type where

import Data.Aeson
import GHC.Generics

newtype LoginParams = LoginParams
    { loginName :: String
    } deriving (Eq, Generic, Read, Show)

instance FromJSON LoginParams
instance ToJSON   LoginParams

newtype Token = Token
    { tokenString :: String
    } deriving (Eq, Generic, Read, Show)

instance FromJSON Token
instance ToJSON   Token
