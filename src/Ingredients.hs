{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Ingredients (createIngredientsTables, handleGetIngredients, handleGetIngredient, handleCreateIngredient, handleGetRecipeIngredients, handleAddRecipeIngredient) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Object, String), object, (.:), (.=))
import Data.Aeson.Types (ToJSON (toJSON), prependFailure, typeMismatch)
import Data.Either (isRight)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Lazy qualified as Lazy
import Database.SQLite.Simple
import Database.SQLite.Simple qualified as SQLite
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import Network.HTTP.Types (status201, status400, status404, status409)
import Util (maybeOccurs, occurences)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)

data MeasurementUnit = Milliliters | Grams | None

unitName :: MeasurementUnit -> Text
unitName Milliliters = "milliliters"
unitName Grams = "grams"
unitName None = "none"

nameToUnit :: Text -> Maybe MeasurementUnit
nameToUnit "milliliters" = Just Milliliters
nameToUnit "grams" = Just Grams
nameToUnit "none" = Just None
nameToUnit _ = Nothing

instance ToJSON MeasurementUnit where
  toJSON = String . unitName

instance ToField MeasurementUnit where
  toField = toField . unitName

data IngredientQuantity = IngredientQuantity
  { measurementUnit :: MeasurementUnit,
    ingredientAmount :: Maybe Int
  }
  deriving (Generic)

data RecipeIngredient = RecipeIngredient
  { ingredientTypeId :: Int,
    recipeId :: Int,
    quantity :: IngredientQuantity
  }
  deriving (Generic)

instance ToJSON RecipeIngredient where
  toJSON (RecipeIngredient ingredType recipe (IngredientQuantity measUnit amt)) =
    object
      [ "ingredientTypeId" .= ingredType,
        "recipeId" .= recipe,
        "measurementUnit" .= measUnit,
        "ingredientAmount" .= amt
      ]

instance ToRow RecipeIngredient where
  toRow (RecipeIngredient ingredType recipe (IngredientQuantity unit amt)) = [toField ingredType, toField recipe, toField unit, toField amt]

instance FromRow RecipeIngredient where
  fromRow = RecipeIngredient <$> field <*> field <*> (IngredientQuantity . fromJust . nameToUnit <$> field <*> field)

data IngredientType = IngredientType {typeId :: Int, ingredientName :: Text} deriving (Generic)

instance FromRow IngredientType

instance ToJSON IngredientType where
  toJSON (IngredientType ingredTypeId typeName) = object ["name" .= typeName, "id" .= ingredTypeId]

createIngredientsTables :: Connection -> IO ()
createIngredientsTables db = do
  execute_ db "CREATE TABLE IF NOT EXISTS ingredientTypes (ingredientTypeId INTEGER PRIMARY KEY AUTOINCREMENT, ingredientName VARCHAR(80) NOT NULL);"
  execute_ db "CREATE TABLE IF NOT EXISTS recipeIngredients (ingredientTypeId INTEGER NOT NULL, recipeId INTEGER NOT NULL, ingredientUnit VARCHAR(30) NOT NULL, ingredientAmount INTEGER, PRIMARY KEY (ingredientTypeId, recipeId));"

createIngredientType :: Connection -> Text -> IO ()
createIngredientType db newIngredientName = do
  execute db "INSERT INTO ingredientTypes (ingredientName) VALUES (?);" (Only newIngredientName)

getIngredientTypes :: Connection -> IO [IngredientType]
getIngredientTypes db = query_ db "SELECT * FROM ingredientTypes;"

getIngredientType :: Connection -> Int -> IO (Maybe IngredientType)
getIngredientType db ingredTypeId = do
  row <- query db "SELECT * FROM ingredientTypes WHERE ingredientTypeId = ?;" (Only ingredTypeId)
  return $ maybeOccurs (occurences row)

addIngredientToRecipe :: Connection -> Int -> Int -> IngredientQuantity -> IO Bool
addIngredientToRecipe db ingredTypeId targetRecipeId qty = do
  let recipeIngred = RecipeIngredient ingredTypeId targetRecipeId qty
  let insertOp = withTransaction db $ execute db "INSERT INTO recipeIngredients VALUES (?, ?, ?, ?);" recipeIngred
  result <- try insertOp :: IO (Either SQLite.SQLError ())
  return $ isRight result

data NamedRecipeIngredient = NamedRecipeIngredient
  { ingredientTypeId :: Int,
    quantity :: IngredientQuantity,
    ingredientName :: Text
  }
  deriving (Generic)

instance FromRow NamedRecipeIngredient where
  fromRow = NamedRecipeIngredient <$> field <*> (IngredientQuantity . fromJust . nameToUnit <$> field <*> field) <*> field

instance ToJSON NamedRecipeIngredient where
  toJSON (NamedRecipeIngredient ingredType (IngredientQuantity measUnit amt) ingredName) =
    object
      [ "ingredientTypeId" .= ingredType,
        "ingredientTypeName" .= ingredName,
        "measurementUnit" .= measUnit,
        "ingredientAmount" .= amt
      ]

getIngredientsOfRecipe :: Connection -> Int -> IO [NamedRecipeIngredient]
getIngredientsOfRecipe db targetRecipeId = query db "SELECT recipeIngredients.ingredientTypeId, ingredientUnit, ingredientAmount, ingredientName FROM recipeIngredients, ingredientTypes WHERE recipeId = ? AND recipeIngredients.ingredientTypeId = ingredientTypes.ingredientTypeId;" (Only targetRecipeId)

handleGetRecipeIngredients :: Connection -> ActionT Lazy.Text IO ()
handleGetRecipeIngredients db = do
  requestedId <- param "recipeId" :: ActionM Int
  ingredients <- liftIO $ getIngredientsOfRecipe db requestedId
  json ingredients

data AddIngredientRequest = AddIngredientRequest {reqIngredientTypeId :: Int, reqUnit :: Text, reqAmount :: Int}

instance FromJSON AddIngredientRequest where
  parseJSON (Object o) = AddIngredientRequest <$> o .: "ingredientTypeId" <*> o .: "measurementUnit" <*> o .: "ingredientAmount"
  parseJSON other = prependFailure "Failed to parse add ingredient request" (typeMismatch "Object" other)

handleAddRecipeIngredient :: Connection -> ActionT Lazy.Text IO ()
handleAddRecipeIngredient db = do
  requestedId <- param "recipeId" :: ActionM Int
  requestData <- jsonData :: ActionM AddIngredientRequest
  case nameToUnit . (.reqUnit) $ requestData of
    Just measUnit -> do
      added <- liftIO $ addIngredientToRecipe db requestData.reqIngredientTypeId requestedId $ IngredientQuantity measUnit (Just requestData.reqAmount)
      if added
        then raiseStatus status201 "Added ingredient to recipe"
        else raiseStatus status409 "Ingredient already exists in recipe"
    Nothing -> raiseStatus status400 "Bad measurement unit"

handleGetIngredients :: Connection -> ActionT Lazy.Text IO ()
handleGetIngredients db = do
  ingredientTypes <- liftIO (getIngredientTypes db)
  json ingredientTypes

handleGetIngredient :: Connection -> ActionT Lazy.Text IO ()
handleGetIngredient db = do
  requestedId <- param "ingredientId" :: ActionM Int
  maybeIngredientType <- liftIO (getIngredientType db requestedId)
  case maybeIngredientType of
    Just ingredientType -> json ingredientType
    Nothing -> raiseStatus status404 "No such ingredient"

newtype CreateIngredientRequest = CreateIngredientRequest {name :: Text} deriving (Generic)

instance FromJSON CreateIngredientRequest where
  parseJSON (Object o) = CreateIngredientRequest <$> o .: "ingredientName"
  parseJSON other = prependFailure "Parsing create ingredient request failed: " (typeMismatch "Object" other)

handleCreateIngredient :: Connection -> ActionT Lazy.Text IO ()
handleCreateIngredient db = do
  createRequest <- jsonData :: ActionM CreateIngredientRequest
  liftIO $ createIngredientType db createRequest.name
  raiseStatus status201 "Created ingredient type"