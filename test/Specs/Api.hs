module Specs.Api where

import Relude
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Either.Combinators
import Sample.Config
import Sample.Database
import Sample.Token
import Servant
import Specs.Utils.Token
import Test.Hspec
--import Test.Hspec.Wai
--import Test.Hspec.Wai.JSON

-- setup :: MyConfig -> (String -> IO ()) -> IO ()
-- setup config action =
--     (`runReaderT` config) $ do
--         mUser <- runSql $ selectUser "tana"
--         user <- case mUser of
--             Just u  -> return u
--             Nothing -> undefined
--         token <- fromRight' <$> getTokenString user
--         lift $ action token

-- specApi :: MyConfig -> Spec
-- specApi config = do
--     around (setup config) $ do
--         describe "logIn" $ do
--             it "success" $ \token -> do
--                 request "POST" "/login" [("Content-Type", "application/json")]
--                     ( encode $ object
--                         [ "logInName"     .= String "tana"
--                         , "logInPassword" .= String "password"
--                         ]
--                     )
--                     `shouldRespondWith`
--                     fromValue
--                         ( object
--                             [ "tokenString" .= token
--                             ]
--                         )

setup :: (String -> IO ()) -> IO ()
setup _ = return ()

spec :: SpecWith ()
spec = do
    around setup $ do
        describe "" $ do
            it "" $ \s -> do
                pending
