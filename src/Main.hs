{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.Monad (when)
import Data.Aeson (decode)
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as B
import Data.Maybe
import Data.Text hiding (map)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Images (createImageTable, handleGetAssociatedImageUUIDs, handleGetImage)
import Ingredients (createIngredientsTables, handleAddRecipeIngredient, handleCreateIngredient, handleGetIngredient, handleGetIngredients, handleGetRecipeIngredients)
import Recipe (createRecipeTable, handleAddRecipe, handleDeleteRecipe, handleGetAllRecipes, handleGetRecipe)
import User (createUser, handleLogin, handleRegister)
import Web.Scotty (delete, get, post, scotty)

createTables :: Connection -> IO ()
createTables db = do
  execute_ db "CREATE TABLE IF NOT EXISTS sessions (token VARCHAR(40) PRIMARY KEY, userId INTEGER NOT NULL, timestamp INTEGER NOT NULL);"
  execute_ db "CREATE TABLE IF NOT EXISTS users (userId INTEGER PRIMARY KEY AUTOINCREMENT, username VARCHAR(255) NOT NULL, passwordHash VARCHAR(80) NOT NULL);"
  createIngredientsTables db
  createRecipeTable db
  createImageTable db

data ServerConfig = ServerConfig {port :: Int, adminPassword :: Text, imageFolder :: String} deriving (Generic)

instance FromJSON ServerConfig

main :: IO ()
main = do
  configBytes <- B.readFile "config.json"
  let config = fromMaybe ServerConfig {port = 3000, adminPassword = "admin", imageFolder = "images"} (decode configBytes)
  db <- open "recipes.sqlite"
  createTables db
  adminSuccess <- createUser db "admin" config.adminPassword
  when adminSuccess $ putStrLn "Created user \"admin\""
  scotty config.port $ do
    post "/register" $ handleRegister db
    post "/login" $ handleLogin db
    get "/ingredients" $ handleGetIngredients db
    post "/ingredients" $ handleCreateIngredient db
    get "/ingredients/:ingredientId" $ handleGetIngredient db
    get "/recipes" $ handleGetAllRecipes db
    get "/recipes/:recipeId" $ handleGetRecipe db
    delete "/recipes/:recipeId" $ handleDeleteRecipe db config.imageFolder
    post "/recipes" $ handleAddRecipe db
    get "/recipes/:recipeId/ingredients" $ handleGetRecipeIngredients db
    post "/recipes/:recipeId/ingredients" $ handleAddRecipeIngredient db
    get "/recipes/:recipeId/images" $ handleGetAssociatedImageUUIDs db
    get "/images/:uuid" $ handleGetImage config.imageFolder