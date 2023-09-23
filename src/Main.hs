{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (decode)
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as B
import Data.Int
import Data.List qualified
import Data.Maybe (fromJust, fromMaybe)
import Data.Password.Bcrypt
import Data.Text
import Data.Text.Internal.Lazy qualified as Lazy
import Data.Text.Lazy (fromStrict, toStrict)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Data.UnixTime (getUnixTime, toEpochTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (ToField (toField))
import Foreign.C.Types (CTime (CTime))
import GHC.Generics (Generic)
import Network.HTTP.Types (status201, status401, status409)
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
    scotty,
  )
import Web.Scotty.Internal.Types (ActionT)

data Color = Color {peter :: String, jonas :: Int} deriving (Generic)

instance ToJSON Color

data Credentials = Credentials {credUsername :: Text, credPassword :: Text}

instance FromJSON Credentials where
  parseJSON (Object o) = Credentials <$> o .: "username" <*> o .: "password"
  parseJSON other = prependFailure "Parsing Credentials failed: " (typeMismatch "Object" other)

createTables :: Connection -> IO ()
createTables db = do
  execute_ db "CREATE TABLE IF NOT EXISTS sessions (token VARCHAR(40) PRIMARY KEY, userId INTEGER NOT NULL, timestamp INTEGER NOT NULL);"
  execute_ db "CREATE TABLE IF NOT EXISTS users (userId INTEGER PRIMARY KEY AUTOINCREMENT, username VARCHAR(40) NOT NULL, passwordHash VARCHAR(80) NOT NULL);"

data DbUser = DbUser {userId :: Int, username :: Text, passwordHash :: Text} deriving (Generic, Show)

instance FromRow DbUser

data DbSession = DbSession {token :: UUID, sessionUserId :: Int, timestamp :: Int64} deriving (Generic)

instance ToRow DbSession where
  toRow DbSession {token = tk, sessionUserId = uid, timestamp = ts} = [toField $ toText tk, toField uid, toField ts]

instance FromRow DbSession where
  fromRow = DbSession . fromJust . fromText <$> field <*> field <*> field

data Occurence a = None | One a | Many

occurences :: [a] -> Occurence a
occurences [] = None
occurences [a] = One a
occurences _ = Many

maybeOccurs :: Occurence a -> Maybe a
maybeOccurs None = Nothing
maybeOccurs Many = Nothing
maybeOccurs (One v) = Just v

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

data ServerConfig = ServerConfig {port :: Int, adminPassword :: Text} deriving (Generic)

instance FromJSON ServerConfig

data RegisterRequest = RegisterRequest {registerUsername :: Text, registerPassword :: Text}

instance FromJSON RegisterRequest where
  parseJSON (Object o) = RegisterRequest <$> o .: "username" <*> o .: "password"
  parseJSON other = prependFailure "Parsing Register Request failed: " (typeMismatch "Object" other)

getTimeSinceEpoch :: IO Int64
getTimeSinceEpoch = do
  epochTime <- toEpochTime <$> getUnixTime
  case epochTime of
    CTime time -> return time

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

data Cookie = Cookie {cookieName :: Text, cookieValue :: Text} deriving (Show)

makeCookieFromList :: [Text] -> Cookie
makeCookieFromList [a, b] = Cookie a b
makeCookieFromList _ = error "Failure constructing cookie from key-value-pair"

extractCookie :: Text -> Text -> Maybe Text
extractCookie cookie cookies = do
  let cookieList = makeCookieFromList . Data.Text.splitOn "=" . Data.Text.strip <$> Data.Text.splitOn ";" cookies
   in cookieValue <$> Data.List.find ((== cookie) . cookieName) cookieList

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

main :: IO ()
main = do
  configBytes <- B.readFile "config.json"
  let config = fromMaybe ServerConfig {port = 3000, adminPassword = "admin"} (decode configBytes)

  db <- open "recipes.sqlite"
  createTables db
  adminSuccess <- addUser db "admin" (adminPassword config)
  when adminSuccess $ putStrLn "Created user \"admin\""
  scotty (port config) $ do
    post "/register" $ do
      _session <- checkSession db
      requestData <- jsonData :: ActionM RegisterRequest
      registerSuccess <- liftIO $ addUser db (registerUsername requestData) (registerPassword requestData)
      if registerSuccess then raiseStatus status201 "Created" else raiseStatus status409 "Could not create"
    post "/login" $ do
      creds <- jsonData :: ActionM Credentials
      userOcc <- liftIO $ findUser db (credUsername creds)
      case userOcc of
        None -> raiseStatus status401 "Invalid Credentials"
        Many -> error "Found multiple Users with same Username. This can never happen"
        One user -> do
          let check = checkPassword (mkPassword $ credPassword creds) (PasswordHash $ passwordHash user)
          if check == PasswordCheckSuccess
            then do
              sessionToken <- liftIO $ createSession db (userId user)
              let headerValue = fromStrict $ pack $ "HSESSIONID=" ++ unpack sessionToken ++ "; HttpOnly"
              addHeader "Set-Cookie" headerValue
              html "Ok"
            else raiseStatus status401 "Invalid Credentials"
    get "/" $ do
      json Color {peter = "peter", jonas = 3}
