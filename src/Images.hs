{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Images
  ( createImageTable,
    getImageFromDisk,
    storeImageInDB,
    handleGetImage,
    handleGetAssociatedImageUUIDs,
  )
where

import Codec.Picture (DynamicImage, readImage)
import Codec.Picture.Saving (imageToJpg)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy qualified as BS (writeFile)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Only (Only), ToRow (toRow), execute, execute_, field, query)
import Database.SQLite.Simple.ToField (ToField (toField))
import Network.HTTP.Types (status400, status404)
import Recipe (RecipeID)
import Text.Read (readMaybe)
import Util (Occurence (Many, None, One))
import Web.Scotty (ActionM, addHeader, json, param, raiseStatus, raw)

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
    \ recipeId INTEGER,\
    \ FOREIGN KEY (recipeId) REFERENCES recipes(recipeId));"

data ImageInfo = ImageInfo
  { uuid :: UUID,
    title :: String,
    recipeId :: RecipeID
  }

instance FromRow ImageInfo where
  fromRow = ImageInfo <$> (read <$> field) <*> field <*> field

instance ToRow ImageInfo where
  toRow (ImageInfo imageUUID imageTitle rId) = [toField . show $ imageUUID, toField imageTitle, toField rId]

imageFolder :: FilePath
imageFolder = "images/"

getImageFromDisk :: UUID -> IO (Maybe DynamicImage)
getImageFromDisk imageUUID = either (const Nothing) Just <$> (readImage . imagePath $ imageUUID)

storeImageOnDisk :: FilePath -> DynamicImage -> IO ()
storeImageOnDisk fPath = BS.writeFile fPath . imageToJpg 50

imagePath :: UUID -> FilePath
imagePath imageUUID = imageFolder ++ show imageUUID ++ ".jpg"

storeImageInDB :: Connection -> String -> DynamicImage -> RecipeID -> IO UUID
storeImageInDB db imageTitle image rId = do
  randomUUID <- nextRandom
  let 
      imageInfo = ImageInfo randomUUID imageTitle rId
  execute db "INSERT INTO images (uuid, title, recipeId) VALUES (?, ?, ?);" . toRow $ imageInfo
  storeImageOnDisk (imagePath randomUUID) image
  return randomUUID

getImageUUIDsForRecipe :: Connection -> RecipeID -> IO [UUID]
getImageUUIDsForRecipe db rId = map uuid <$> query db "SELECT * FROM images WHERE recipeId = ?;" (Only rId)

handleGetImage :: ActionM ()
handleGetImage = do
  maybeImageUUID <- readMaybe <$> param "uuid" :: ActionM (Maybe UUID)
  imageUUID <- maybe (raiseStatus status400 "Invalid uuid.") return maybeImageUUID
  imageOcc <- liftIO $ getImageFromDisk imageUUID
  case imageOcc of
    Nothing -> raiseStatus status404 "Unknown image."
    Just image -> do
      addHeader "Content-Type" "image/jpeg"
      addHeader "Cache-Control" "public, max-age=15552000"
      raw . imageToJpg 50 $ image

handleGetAssociatedImageUUIDs :: Connection -> ActionM ()
handleGetAssociatedImageUUIDs db = do
  rId <- param "recipeId" :: ActionM Int
  imageUUIDs <- liftIO $ getImageUUIDsForRecipe db rId
  json imageUUIDs