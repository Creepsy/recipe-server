{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module User (handleRegister, handleLogin, createUser) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Aeson.Types
import Data.Int
import Data.Maybe
import Data.Password.Bcrypt
import Data.Text
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy qualified as Lazy
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import GHC.Generics
import Network.HTTP.Types (status201, status401, status409)
import Util
import Web.Scotty
import Web.Scotty.Internal.Types

data DbSession = DbSession {token :: UUID, sessionUserId :: Int, timestamp :: Int64} deriving (Generic)

instance ToRow DbSession where
  toRow DbSession {token = tk, sessionUserId = uid, timestamp = ts} = [toField $ toText tk, toField uid, toField ts]

instance FromRow DbSession where
  fromRow = DbSession . fromJust . fromText <$> field <*> field <*> field

data DbUser = DbUser {userId :: Int, username :: Text, passwordHash :: Text} deriving (Generic, Show)

instance FromRow DbUser

data RegisterRequest = RegisterRequest {registerUsername :: Text, registerPassword :: Text}

instance FromJSON RegisterRequest where
  parseJSON (Object o) = RegisterRequest <$> o .: "username" <*> o .: "password"
  parseJSON other = prependFailure "Parsing Register Request failed: " (typeMismatch "Object" other)

data LoginRequest = LoginRequest {loginUsername :: Text, loginPassword :: Text}

instance FromJSON LoginRequest where
  parseJSON (Object o) = LoginRequest <$> o .: "username" <*> o .: "password"
  parseJSON other = prependFailure "Parsing Credentials failed: " (typeMismatch "Object" other)

findUser :: Connection -> Text -> IO (Occurence DbUser)
findUser db name = do
  row <- query db "SELECT * FROM users WHERE username = ?;" (Only (name :: Text)) :: IO [DbUser]
  return $ occurences row

createUser :: Connection -> Text -> Text -> IO Bool
createUser db newUsername newPassword =
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

createSession :: Connection -> Int -> IO Text
createSession db loggedInUserId = do
  currentTimestamp <- getTimeSinceEpoch
  randomToken <- nextRandom
  _ <- execute db "INSERT INTO sessions (token, userId, timestamp) VALUES (?, ?, ?);" $ DbSession randomToken loggedInUserId currentTimestamp
  return $ toText randomToken

findSession :: Connection -> Text -> IO (Occurence DbSession)
findSession db sessionToken = do
  row <- query db "SELECT * FROM sessions WHERE token = ?;" (Only (sessionToken :: Text)) :: IO [DbSession]
  return $ occurences row

checkSession :: Connection -> ActionT Lazy.Text IO DbSession
checkSession db = do
  maybeCookies <- header "Cookie"
  case maybeCookies >>= extractCookie "HSESSIONID" . toStrict of
    Just sessionCookie -> do
      sessionOcc <- liftIO $ findSession db sessionCookie
      case maybeOccurs sessionOcc of
        Just session -> return session
        Nothing -> raiseStatus status401 "Invalid session cookie"
    Nothing -> raiseStatus status401 "Invalid session cookie"

handleRegister :: Connection -> ActionT Lazy.Text IO ()
handleRegister db = do
  _session <- checkSession db
  requestData <- jsonData :: ActionM RegisterRequest
  registerSuccess <- liftIO $ createUser db (registerUsername requestData) (registerPassword requestData)
  if registerSuccess then raiseStatus status201 "Created" else raiseStatus status409 "Could not create"

handleLogin :: Connection -> ActionT Lazy.Text IO ()
handleLogin db = do
  creds <- jsonData :: ActionM LoginRequest
  userOcc <- liftIO $ findUser db (loginUsername creds)
  case userOcc of
    None -> raiseStatus status401 "Invalid Credentials"
    Many -> error "Found multiple Users with same Username. This can never happen"
    One user -> do
      let check = checkPassword (mkPassword $ loginPassword creds) (PasswordHash $ passwordHash user)
      if check == PasswordCheckSuccess
        then do
          sessionToken <- liftIO $ createSession db (userId user)
          let headerValue = fromStrict $ pack $ "HSESSIONID=" ++ unpack sessionToken ++ "; HttpOnly"
          addHeader "Set-Cookie" headerValue
          html "Ok"
        else raiseStatus status401 "Invalid Credentials"