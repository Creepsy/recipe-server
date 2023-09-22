{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.Types
import Data.Password.Bcrypt
import Data.Text
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Network.HTTP.Types (status401)
import Web.Scotty

data Color = Color {peter :: String, jonas :: Int} deriving (Generic)

instance ToJSON Color

data Credentials = Credentials {credUsername :: Text, credPassword :: Text}

instance FromJSON Credentials where
  parseJSON (Object o) = Credentials <$> o .: "username" <*> o .: "password"
  parseJSON other = prependFailure "Parsing Credentials failed: " (typeMismatch "Object" other)

createTables :: Connection -> IO ()
createTables db = do
  execute_ db "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username VARCHAR(40) NOT NULL, passwordHash VARCHAR(80) NOT NULL);"

data DbUser = DbUser {id :: Int, username :: Text, passwordHash :: Text} deriving (Generic, Show)

instance FromRow DbUser

data Occurence a = None | One a | Many

occurences :: [a] -> Occurence a
occurences [] = None
occurences [a] = One a
occurences _ = Many

findUser :: Connection -> Text -> IO (Occurence DbUser)
findUser db name = do
  row <- query db "SELECT * FROM users WHERE username = ?;" (Only (name :: Text)) :: IO [DbUser]
  return $ occurences row

addUser :: Connection -> Text -> Text -> IO Bool
addUser db newUsername newPassword =
  do
    row <- query db "SELECT * FROM users WHERE username = ?;" (Only (newUsername :: Text)) :: IO [DbUser]
    case row of
      [] -> do
        hash <- hashPassword $ mkPassword newPassword
        let hashText = unPasswordHash hash
        execute
          db
          "INSERT INTO users (username, passwordHash) VALUES (?, ?);"
          [newUsername :: Text, hashText :: Text]
        return True
      _ -> return False

main :: IO ()
main = do
  db <- open "recipes.sqlite"
  createTables db
  success <- addUser db "ian" "ian"
  when success $ putStrLn "Created user \"ian\""
  scotty 3000 $ do
    post "/login" $ do
      creds <- jsonData :: ActionM Credentials
      userOcc <- liftIO $ findUser db (credUsername creds)
      case userOcc of
        None -> raiseStatus status401 "Invalid Credentials"
        Many -> error "Found multiple Users with same Username. This can never happen"
        One user -> do
          let check = checkPassword (mkPassword $ credPassword creds) (PasswordHash $ passwordHash user)
          if check == PasswordCheckSuccess then html "Ok" else raiseStatus status401 "Invalid Credentials"
    get "/" $ do
      json Color {peter = "peter", jonas = 3}
