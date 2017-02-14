{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Control.Monad (forM_, mzero)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Crypto.PasswordStore as PW
import Data.Aeson hiding (json)
import Data.ByteString hiding (pack, unpack)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Calendar
import Data.Time.Clock
import Database.Persist
import Database.Persist.Sqlite as Sqlite
import Database.Persist.TH
import Web.Scotty

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
WorkDay json
    start UTCTime
    minutes Int
    deriving Show

User json
    username String
    usertoken String
    deriving Show
|]

data WorkDayAction = WorkDayAction
  { actStart :: Maybe UTCTime
  , actMinutes :: Maybe Int
  } deriving (Show)

data NewUser = NewUser
  { newUsername :: String
  , newPassword :: String
  } deriving (Show)

instance FromJSON WorkDayAction where
  parseJSON (Object o) = WorkDayAction <$> o .:? "start" <*> o .:? "minutes"
  parseJSON _ = mzero

instance FromJSON NewUser where
  parseJSON (Object o) = NewUser <$> o .: "username" <*> o .: "password"
  parseJSON _ = mzero

instance ToJSON NewUser where
  toJSON p = object ["username" .= newUsername p, "password" .= newPassword p]

actionToMyday :: WorkDayAction -> WorkDay
actionToMyday (WorkDayAction mStart mMinutes) = WorkDay start minutes
  where
    start = fromMaybe (UTCTime (fromGregorian 0 0 0) 0) mStart
    minutes = fromMaybe 0 mMinutes

actionToUser :: NewUser -> User
actionToUser (NewUser username password) = User username (unpack token)
  where
    token = PW.makePasswordSalt (pack password) (makeSalt "thisIsTheSalt") 22

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn

insertDates :: WorkDay -> IO (Key WorkDay)
insertDates day = runDb $ Sqlite.insert day

readDates :: IO [Entity WorkDay]
readDates = runDb $ selectList [] [LimitTo 10]

insertUser :: NewUser -> IO (Key User)
insertUser newUser = runDb $ Sqlite.insert $ actionToUser newUser

readUser :: String -> IO (Maybe (Entity User))
readUser username = runDb $ selectFirst [UserUsername ==. username] []

migrate :: IO ()
migrate = runDb $ Sqlite.runMigration migrateAll

authenticate :: (User -> ActionM ()) -> ActionM ()
authenticate = undefined
