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
import           Data.Aeson                   hiding (json)
import           Data.Maybe                   (fromMaybe)
import           Data.Time                    (UTCTime, getCurrentTime)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite      as Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
WorkDay json
    start UTCTime
    minutes Int
    deriving Show
|]

data WorkDayAction = WorkDayAction
  { actStart   :: Maybe UTCTime
  , actMinutes :: Maybe Int
  } deriving Show

instance FromJSON WorkDayAction where
  parseJSON (Object o) = WorkDayAction
    <$> o .:? "start"
    <*> o .:? "minutes"
  parseJSON _ = mzero

actionToMyday :: WorkDayAction -> WorkDay
actionToMyday (WorkDayAction mStart mMinutes ) = WorkDay start minutes
  where
    start = fromMaybe (UTCTime (fromGregorian 0 0 0) 0) mStart
    minutes = fromMaybe 0 mMinutes

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn

insertDates :: WorkDay -> IO (Key WorkDay)
insertDates day = runDb $ Sqlite.insert day

readDates :: IO [Entity WorkDay]
readDates = runDb $ selectList [] [LimitTo 10]

migrate :: IO ()
migrate = runDb $ Sqlite.runMigration migrateAll
