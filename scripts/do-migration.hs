module Main where

import Relude
import Sample.Migrations

main :: IO ()
main = doMigration migrateAll "./config/development/sample.sqlite3"
