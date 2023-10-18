{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Recipe
  ( createRecipeTable,
    Ingredient (..),
    Recipe (..),
    RecipeID,
    handleGetAllRecipePreviews,
    handleGetRecipe,
    handleAddRecipe,
    handleDeleteRecipe,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID, nil)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Only (Only), ToRow (toRow), changes, execute, execute_, field, query, query_, (:.) ((:.)))
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Images (deleteImageFromDB, getAssociatedImageUUIDs)
import Network.HTTP.Types (status201, status404)
import TypeAliases (RecipeID)
import Util (Occurence (Many, None, One), occurences)
import Web.Scotty (ActionM, html, json, jsonData, param, raiseStatus)

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

data RecipePreview = RecipePreview
  { recipeId :: RecipeID,
    title :: String,
    titleImage :: UUID,
    timeMinutes :: Int,
    costCents :: Int,
    stars :: Int
  }
  deriving (Generic)

instance ToJSON RecipePreview

handleGetAllRecipePreviews :: Connection -> ActionM ()
handleGetAllRecipePreviews db = liftIO (getAllRecipePreviews db) >>= json

getAllRecipePreviews :: Connection -> IO [RecipePreview]
getAllRecipePreviews db = getAllRecipes db >>= mapM (recipeToPreview db)

recipeToPreview :: Connection -> (RecipeID, Recipe) -> IO RecipePreview
recipeToPreview db (recipeId, recipe) = do
  associatedImages <- getAssociatedImageUUIDs db recipeId
  let titleImage = case associatedImages of
        (tI : _) -> tI
        _ -> nil
  return $ RecipePreview recipeId recipe.title titleImage recipe.timeMinutes recipe.costCents recipe.stars

handleAddRecipe :: Connection -> ActionM ()
handleAddRecipe db = do
  recipeToAdd <- jsonData :: ActionM Recipe
  liftIO $ addRecipe db recipeToAdd
  raiseStatus status201 "Recipe inserted into database!"

handleDeleteRecipe :: Connection -> FilePath -> ActionM ()
handleDeleteRecipe db imageFolder = do
  recipeId <- param "recipeId" :: ActionM RecipeID
  associatedImages <- liftIO $ getAssociatedImageUUIDs db recipeId
  liftIO $ forM_ associatedImages (deleteImageFromDB db imageFolder)
  _ <- liftIO $ execute db "DELETE FROM recipes WHERE recipeId = ?;" (Only recipeId)
  rowsDeleted <- liftIO $ changes db
  case rowsDeleted of
    1 -> html ""
    _ -> raiseStatus status404 "No recipe with ID found"

getAllRecipes :: Connection -> IO [(RecipeID, Recipe)]
getAllRecipes db = map convertToTuple <$> (query_ db "SELECT * FROM recipes;" :: IO [Only RecipeID :. Recipe])
  where
    convertToTuple (Only recipeId :. recipe) = (recipeId, recipe)

data RecipeWithAssociations = RecipeWithAssociations
  { recipeId :: RecipeID,
    title :: String,
    images :: [UUID],
    timeMinutes :: Int,
    costCents :: Int,
    stars :: Int
  }
  deriving (Generic)

instance ToJSON RecipeWithAssociations

handleGetRecipe :: Connection -> ActionM ()
handleGetRecipe db = do
  recipeID <- param "recipeId"
  recipeOcc <- liftIO $ getRecipeWithAssociations db recipeID
  case recipeOcc of
    None -> raiseStatus status404 "Unknown recipe."
    One recipeToGet -> json recipeToGet
    Many -> error "Found multiple recipes with same id. This can never happen."

getRecipeWithAssociations :: Connection -> RecipeID -> IO (Occurence RecipeWithAssociations)
getRecipeWithAssociations db recipeId = getRecipe db recipeId >>= traverse addAssociationsToRecipe
  where
    addAssociationsToRecipe :: Recipe -> IO RecipeWithAssociations
    addAssociationsToRecipe recipe = do
      associatedImages <- getAssociatedImageUUIDs db recipeId
      return $ RecipeWithAssociations recipeId recipe.title associatedImages recipe.timeMinutes recipe.costCents recipe.stars

getRecipe :: Connection -> RecipeID -> IO (Occurence Recipe)
getRecipe db recipeID =
  occurences . map (\(Only _ :. recipe) -> recipe)
    <$> (query db "SELECT * FROM recipes WHERE recipeId = ?;" (Only recipeID) :: IO [Only RecipeID :. Recipe])

addRecipe :: Connection -> Recipe -> IO ()
addRecipe db recipeToAdd = do
  execute
    db
    "INSERT INTO recipes (title, description, ingredients, timeMinutes, costCents, stars) VALUES (?, ?, ?, ?, ?, ?);"
    $ toRow recipeToAdd