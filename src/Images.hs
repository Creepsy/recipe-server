{-# LANGUAGE OverloadedStrings #-}

module Images
  ( createImageTable,
    getImageFromDB,
    storeImageInDB,
    getImageUUIDsForRecipe,
    handleGetImage,
    handleGetAssociatedImageUUIDs
  )
where

import Codec.Picture (DynamicImage, readImage)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Only (Only), ToRow (toRow), execute, execute_, field, query)
import Database.SQLite.Simple.ToField (ToField (toField))
import Recipe (RecipeID)
import Util (Occurence (One, Many, None), occurences)
import Web.Scotty (ActionM, param, raiseStatus, setHeader, addHeader, raw, json)
import Codec.Picture.Saving (imageToJpg)
import qualified Data.ByteString.Lazy as BS (writeFile)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Network.HTTP.Types (status404, status400)
import Text.Read (readMaybe)
import Data.Maybe (fromJust, fromMaybe)

createImageTable :: Connection -> IO ()
createImageTable db = do
  {-
    Image:
      uuid:     UUID of the image
      title:    Title of the image
      filePath: Filepath to the image
      recipeID: ID of the associated recipe
  -}
  execute_
    db
    "CREATE TABLE IF NOT EXISTS images (\
    \ uuid VARCHAR(36) PRIMARY KEY NOT NULL,\
    \ title VARCHAR(40) NOT NULL,\
    \ filePath VARCHAR(100) NOT NULL,\
    \ recipeId INTEGER,\
    \ FOREIGN KEY (recipeId) REFERENCES recipes(recipeId));"

data ImageInfo = ImageInfo
  { uuid :: UUID,
    title :: String,
    filePath :: FilePath,
    recipeId :: RecipeID
  }

instance FromRow ImageInfo where
  fromRow = ImageInfo <$> (read <$> field) <*> field <*> field <*> field

instance ToRow ImageInfo where
  toRow (ImageInfo uuid title filePath recipeId) = [toField . show $ uuid, toField title, toField filePath, toField recipeId]

imageFolder :: FilePath
imageFolder = "images/"

getImageFromDB :: Connection -> UUID -> IO (Occurence DynamicImage)
getImageFromDB db uuid = occurences <$> (traverse readImageFromDisk =<< query db "SELECT * FROM images WHERE uuid = ?;" (Only . show $ uuid))
  where
    readImageFromDisk info = either error id <$> (readImage . filePath $ info)

storeImageOnDisk :: FilePath -> DynamicImage -> IO ()
storeImageOnDisk filePath = BS.writeFile filePath . imageToJpg 50

storeImageInDB :: Connection -> String -> DynamicImage -> RecipeID -> IO UUID
storeImageInDB db title image recipeId = do
  randomUUID <- nextRandom
  let filePath = imageFolder ++ show randomUUID ++ ".jpg"
      imageInfo = ImageInfo randomUUID title filePath recipeId
  execute db "INSERT INTO images (uuid, title, filePath, recipeId) VALUES (?, ?, ?, ?);" . toRow $ imageInfo
  storeImageOnDisk filePath image
  return randomUUID

getImageUUIDsForRecipe :: Connection -> RecipeID -> IO [UUID]
getImageUUIDsForRecipe db recipeId = map uuid <$> query db "SELECT * FROM images WHERE recipeId = ?;" (Only recipeId)

handleGetImage :: Connection -> ActionM ()
handleGetImage db = do
  maybeImageUUID <- readMaybe <$> param "uuid" :: ActionM (Maybe UUID)
  imageUUID <- maybe (raiseStatus status400 "Invalid uuid.") return maybeImageUUID
  imageOcc <- liftIO $ getImageFromDB db imageUUID
  case imageOcc of
    None -> raiseStatus status404 "Unknown image."
    One image -> do
      addHeader "Content-Type" "image/jpeg" 
      addHeader "Cache-Control" "public, max-age=15552000"
      raw . imageToJpg 50 $ image
    Many -> error "Multiple images with the same UUID found" 

handleGetAssociatedImageUUIDs :: Connection -> ActionM ()
handleGetAssociatedImageUUIDs db = do
  recipeId <- param "recipeId" :: ActionM Int
  imageUUIDs <- liftIO $ getImageUUIDsForRecipe db recipeId
  json imageUUIDs