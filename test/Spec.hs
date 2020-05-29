module Main where

import Relude
import Control.Exception
import Control.Monad.Reader
import Sample.App
import Specs.Api
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main =
    -- bracket (initialize AppEnvTest) destroy $
    --     \config ->
    --         (`runReaderT` config) . hspec . with (return $ app config) $ do
    --             specApi
    return ()
