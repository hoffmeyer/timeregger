{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Model where

import           Control.Monad                (forM_, mzero)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Crypto.PasswordStore         as PW
import           Data.Aeson                   hiding (json)
import           Data.ByteString              hiding (pack, unpack)
import           Data.ByteString.Char8        (pack, unpack)
import           Data.Int
import           Data.Maybe                   (fromMaybe)
import           Data.Time                    (UTCTime, getCurrentTime)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite      as Sqlite
import           Database.Persist.TH
import           Web.Scotty

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User json
    username String
    usertoken String
    workWeek Int
    deriving Show

WorkDay json
    user UserId      -- required Foreign Key
    start UTCTime
    minutes Int
    deriving Show
|]

data WorkDayAction = WorkDayAction
  { actStart   :: Maybe UTCTime
  , actMinutes :: Maybe Int
  } deriving (Show)

data NewUser = NewUser
  { newUsername :: String
  , newPassword :: String
  , newWorkWeek :: Maybe Int
  } deriving (Show)

instance FromJSON WorkDayAction where
  parseJSON (Object o) = WorkDayAction <$> o .:? "start" <*> o .:? "minutes"
  parseJSON _          = mzero

instance FromJSON NewUser where
  parseJSON (Object o) = NewUser <$> o .: "username" <*> o .: "password" <*> o .:? "workWeek"
  parseJSON _          = mzero

instance ToJSON NewUser where
  toJSON p = object ["username" .= newUsername p, "password" .= newPassword p]

actionToMyday :: WorkDayAction -> Entity User -> WorkDay
actionToMyday (WorkDayAction mStart mMinutes) (Entity uid _) = WorkDay uid start minutes
  where
    start = fromMaybe (UTCTime (fromGregorian 0 0 0) 0) mStart
    minutes = fromMaybe 0 mMinutes

actionToUser :: NewUser -> User
actionToUser (NewUser username password mWorkWeek) = User username (unpack token) workWeek
  where
    token = PW.makePasswordSalt (pack password) (makeSalt "thisIsTheSalt") 22
    workWeek = fromMaybe 0 mWorkWeek

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn

insertDates :: WorkDay -> IO (Key WorkDay)
insertDates day = runDb $ Sqlite.insert day

updateDate :: WorkDay -> Int64 -> IO ()
updateDate day id = runDb $ Sqlite.replace (toSqlKey id) day

deleteDate :: Int64 -> IO ()
deleteDate id = runDb $ Sqlite.delete (toSqlKey id :: Key WorkDay)

readDates :: IO [Entity WorkDay]
readDates = runDb $ selectList [] [LimitTo 10]

readDate :: Int64 -> IO (Maybe WorkDay)
readDate id = runDb $ Sqlite.get (toSqlKey id :: Key WorkDay)

insertUser :: NewUser -> IO (Key User)
insertUser newUser = runDb $ Sqlite.insert $ actionToUser newUser

readUser :: String -> IO (Maybe (Entity User))
readUser username = runDb $ selectFirst [UserUsername ==. username] []

deleteUser :: Key User -> IO ()
deleteUser key = runDb $ Sqlite.delete key

migrate :: IO ()
migrate = runDb $ Sqlite.runMigration migrateAll

authenticate :: (User -> ActionM ()) -> ActionM ()
authenticate = undefined
