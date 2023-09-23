{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Ingredients (createIngredientsTables, handleGetIngredients, handleGetIngredient, handleCreateIngredient) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Object), object, (.:), (.=))
import Data.Aeson.Types (ToJSON (toJSON), prependFailure, typeMismatch)
import Data.Text (Text)
import Data.Text.Lazy qualified as Lazy
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Network.HTTP.Types (status201, status404)
import Util (maybeOccurs, occurences)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)

data IngredientType = IngredientType {ingredientTypeId :: Int, ingredientName :: Text} deriving (Generic)

instance FromRow IngredientType

instance ToJSON IngredientType where
  toJSON (IngredientType typeId typeName) = object ["name" .= typeName, "id" .= typeId]

createIngredientsTables :: Connection -> IO ()
createIngredientsTables db = do
  execute_ db "CREATE TABLE IF NOT EXISTS ingredientTypes (ingredientTypeId INTEGER PRIMARY KEY AUTOINCREMENT, ingredientName VARCHAR(80) NOT NULL);"

createIngredientType :: Connection -> Text -> IO ()
createIngredientType db newIngredientName = do
  execute db "INSERT INTO ingredientTypes (ingredientName) VALUES (?);" (Only newIngredientName)

getIngredientTypes :: Connection -> IO [IngredientType]
getIngredientTypes db = query_ db "SELECT * FROM ingredientTypes;"

getIngredientType :: Connection -> Int -> IO (Maybe IngredientType)
getIngredientType db typeId = do
  row <- query db "SELECT * FROM ingredientTypes WHERE ingredientTypeId = ?;" (Only typeId)
  return $ maybeOccurs (occurences row)

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
  liftIO $ createIngredientType db $ name createRequest
  raiseStatus status201 "Created ingredient type"