{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.Aeson (decode)
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as B
import Data.Maybe
import Data.Text
import Database.SQLite.Simple (Connection, execute_, open)
import GHC.Generics (Generic)
import Ingredients (createIngredientsTables, handleCreateIngredient, handleGetIngredient, handleGetIngredients)
import User (createUser, handleLogin, handleRegister)
import Web.Scotty (scotty, post, get)

createTables :: Connection -> IO ()
createTables db = do
  execute_ db "CREATE TABLE IF NOT EXISTS sessions (token VARCHAR(40) PRIMARY KEY, userId INTEGER NOT NULL, timestamp INTEGER NOT NULL);"
  execute_ db "CREATE TABLE IF NOT EXISTS users (userId INTEGER PRIMARY KEY AUTOINCREMENT, username VARCHAR(255) NOT NULL, passwordHash VARCHAR(80) NOT NULL);"
  createIngredientsTables db

data ServerConfig = ServerConfig {port :: Int, adminPassword :: Text} deriving (Generic)

instance FromJSON ServerConfig

main :: IO ()
main = do
  configBytes <- B.readFile "config.json"
  let config = fromMaybe ServerConfig {port = 3000, adminPassword = "admin"} (decode configBytes)

  db <- open "recipes.sqlite"
  createTables db
  adminSuccess <- createUser db "admin" (adminPassword config)
  when adminSuccess $ putStrLn "Created user \"admin\""
  scotty (port config) $ do
    post "/register" $ handleRegister db
    post "/login" $ handleLogin db
    get "/ingredients" $ handleGetIngredients db
    post "/ingredients" $ handleCreateIngredient db
    get "/ingredients/:ingredientId" $ handleGetIngredient db
