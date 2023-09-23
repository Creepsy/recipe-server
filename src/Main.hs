{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (catch)
import Control.Monad (when)
import Data.Aeson (decode)
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as B
import Data.Maybe
import Ingredients (createIngredientsTables, handleCreateIngredient, handleGetIngredient, handleGetIngredients)
import User (createUser, handleLogin, handleRegister)
import Data.Text hiding (map)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Network.HTTP.Types (status401, status404, status201)
import Web.Scotty
  ( ActionM,
    addHeader,
    get,
    header,
    html,
    json,
    jsonData,
    post,
    raiseStatus,
    scotty, param,
  )
import Util (Occurence(None, Many, One), occurences)
import Control.Monad.IO.Class (MonadIO(liftIO))

data Color = Color {peter :: String, jonas :: Int} deriving (Generic)

instance ToJSON Color

data Credentials = Credentials {credUsername :: Text, credPassword :: Text}

instance FromJSON Credentials where
  parseJSON (Object o) = Credentials <$> o .: "username" <*> o .: "password"
  parseJSON other = prependFailure "Parsing Credentials failed: " (typeMismatch "Object" other)

createTables :: Connection -> IO ()
createTables db = do
  execute_ db "CREATE TABLE IF NOT EXISTS sessions (token VARCHAR(40) PRIMARY KEY, userId INTEGER NOT NULL, timestamp INTEGER NOT NULL);"
  execute_ db "CREATE TABLE IF NOT EXISTS users (userId INTEGER PRIMARY KEY AUTOINCREMENT, username VARCHAR(255) NOT NULL, passwordHash VARCHAR(80) NOT NULL);"
  createIngredientsTables db
  {-
    Recipe:
      id:           unique id for identification of recipes
      title:        Name of the recipe (40 chars max)
      description:  Description of the recipe
      ingredients:  Comma-seperated list of ingredients stored as string (in Haskell format)
      timeMinutes:  time in minutes
      costCents:    cost in cents
      stars:        integer between 0 and 5
  -}
  execute_
    db
    "CREATE TABLE IF NOT EXISTS recipes (\
    \ recipeId INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ title VARCHAR(40) NOT NULL,\
    \ description TEXT NOT NULL,\
    \ ingredients TEXT NOT NULL,\
    \ timeMinutes INTEGER NOT NULL,\
    \ costCents INTEGER NOT NULL,\
    \ stars INTEGER NOT NULL);"

data ServerConfig = ServerConfig {port :: Int, adminPassword :: Text} deriving (Generic)

instance FromJSON ServerConfig

data Ingredient = Ingredient {count :: Int, ingredient :: String} deriving (Read, Show, Generic)

instance ToJSON Ingredient

instance FromJSON Ingredient

data Recipe = Recipe
  { title :: String,
    description :: String,
    ingredients :: [Ingredient],
    timeMinutes :: Int,
    costCents :: Int,
    stars :: Int
  }
  deriving (Generic)

instance ToJSON Recipe

instance FromJSON Recipe

instance FromRow Recipe where
  fromRow = Recipe <$> field <*> field <*> (read <$> field) <*> field <*> field <*> field

instance ToRow Recipe where
  toRow (Recipe t d i tM cC s) =
    [ toField t,
      toField d,
      toField . show $ i,
      toField tM,
      toField cC,
      toField s
    ]

type RecipeID = Int

data RecipeWithID = RecipeWithID {recipeId :: Int, recipe :: Recipe}

instance FromRow RecipeWithID where
  fromRow = RecipeWithID <$> field <*> fromRow

instance ToJSON RecipeWithID where
  toJSON (RecipeWithID rID (Recipe t d i tM cC s)) =
    object
      [ "id" .= rID,
        "title" .= t,
        "description" .= d,
        "ingredients" .= i,
        "timeMinutes" .= tM,
        "costCents" .= cC,
        "stars" .= s
      ]

getAllRecipes :: Connection -> IO [RecipeWithID]
getAllRecipes = flip query_ "SELECT * FROM recipes;"

addRecipe :: Connection -> Recipe -> IO ()
addRecipe db recipe =
  do
    execute
      db
      "INSERT INTO recipes (title, description, ingredients, timeMinutes, costCents, stars) VALUES (?, ?, ?, ?, ?, ?);"
      $ toRow recipe

getRecipe :: Connection -> RecipeID -> IO (Occurence Recipe)
getRecipe db recipeID = occurences . map recipe <$> query db "SELECT * FROM recipes WHERE id = ?;" (Only recipeID)

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
    get "/recipes" $ json =<< liftIO (getAllRecipes db)
    post "/recipes" $ do
      recipe <- jsonData :: ActionM Recipe
      liftIO $ addRecipe db recipe
      raiseStatus status201 "Recipe inserted into database!"
    get "/recipes/:recipeID" $ do
      recipeID <- param "recipeID"
      recipeOcc <- liftIO $ getRecipe db recipeID
      case recipeOcc of
        None -> raiseStatus status404 "Unknown recipe."
        One recipe -> json recipe
        Many -> error "Found multiple recipes with same id. This can never happen."
