module Sample.Api.GetAllUsers where

import Database.Persist
import Sample.Api.Types
import Sample.Database
import Sample.Migrations

getAllUsers :: Entity User -> MyHandler [Entity User]
getAllUsers _ = runSql selectAllUsers
