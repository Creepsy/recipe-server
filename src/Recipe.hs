{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Recipe
  ( createRecipeTable,
    Ingredient (..),
    Recipe (..),
    RecipeID,
    RecipeWithID (..),
    handleGetAllRecipes,
    handleGetRecipe,
    handleAddRecipe,
    getAllRecipes,
    getRecipe,
    addRecipe,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON (toJSON), object, (.=))
import Database.SQLite.Simple (Connection, FromRow (fromRow), Only (Only), ToRow (toRow), execute, execute_, field, query, query_)
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Network.HTTP.Types (status201, status404)
import Util (Occurence (Many, None, One), occurences)
import Web.Scotty (ActionM, json, jsonData, param, raiseStatus)

createRecipeTable :: Connection -> IO ()
createRecipeTable db = do
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

handleGetAllRecipes :: Connection -> ActionM ()
handleGetAllRecipes db = liftIO (getAllRecipes db) >>= json

handleGetRecipe :: Connection -> ActionM ()
handleGetRecipe db = do
  recipeID <- param "recipeID"
  recipeOcc <- liftIO $ getRecipe db recipeID
  case recipeOcc of
    None -> raiseStatus status404 "Unknown recipe."
    One recipeToGet -> json recipeToGet
    Many -> error "Found multiple recipes with same id. This can never happen."

handleAddRecipe :: Connection -> ActionM ()
handleAddRecipe db = do
  recipeToAdd <- jsonData :: ActionM Recipe
  liftIO $ addRecipe db recipeToAdd
  raiseStatus status201 "Recipe inserted into database!"

getAllRecipes :: Connection -> IO [RecipeWithID]
getAllRecipes = flip query_ "SELECT * FROM recipes;"

getRecipe :: Connection -> RecipeID -> IO (Occurence Recipe)
getRecipe db recipeID = occurences . map recipe <$> query db "SELECT * FROM recipes WHERE id = ?;" (Only recipeID)

addRecipe :: Connection -> Recipe -> IO ()
addRecipe db recipeToAdd = do
  execute
    db
    "INSERT INTO recipes (title, description, ingredients, timeMinutes, costCents, stars) VALUES (?, ?, ?, ?, ?, ?);"
    $ toRow recipeToAdd