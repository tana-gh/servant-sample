module Main where

import Relude
import Control.Exception
import Network.Wai.Handler.Warp
import Sample.App

main :: IO ()
main =
    bracket (initialize AppEnvDevelopment) destroy $
        \config -> do
            putStrLn "Listening on port 8080"
            run 8080 $ app config
