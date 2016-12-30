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
MyDay json
    start UTCTime
    minutes Int
    deriving Show
|]

data MyDayAction = MyDayAction
  { actStart   :: Maybe UTCTime
  , actMinutes :: Maybe Int
  } deriving Show

instance FromJSON MyDayAction where
  parseJSON (Object o) = MyDayAction
    <$> o .:? "start"
    <*> o .:? "minutes"
  parseJSON _ = mzero

actionToMyday :: MyDayAction -> MyDay
actionToMyday (MyDayAction mStart mMinutes ) = MyDay start minutes
  where
    start = fromMaybe (UTCTime (fromGregorian 0 0 0) 0) mStart
    minutes = fromMaybe 0 mMinutes

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn

insertDates :: MyDay -> IO (Key MyDay)
insertDates day = runDb $ Sqlite.insert day

readDates :: IO [Entity MyDay]
readDates = runDb $ selectList [] [LimitTo 10]

migrate :: IO ()
migrate = runDb $ Sqlite.runMigration migrateAll
