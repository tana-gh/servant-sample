module Main where

import Relude
import Sample.Key

main :: IO ()
main = generateKeyFile "./config/development/.key"
