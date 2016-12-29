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

module Main where

import           Control.Applicative
import           Control.Monad                (forM_, mzero)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   hiding (json)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid
import           Data.Time                    (UTCTime, getCurrentTime)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite      as Sqlite
import           Database.Persist.TH
import           Lib
import qualified Web.Scotty                   as S


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

readDates :: IO [Entity MyDay]
readDates = runDb $ selectList [] [LimitTo 10]

today :: MyDay
today = MyDay (UTCTime (fromGregorian 2011 12 16) (fromIntegral $ 12 * 3600)) 450

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  S.scotty 3000 $ do
    S.get "/" $ do
      S.html "Hello World!"
    S.get "/hello/:name" $ do
      name <- S.param "name"
      S.text ("hello " <> name <> "!")
    S.get "/dates" $ do
      todos <- liftIO readDates
      S.json todos
    S.put "/dates" $ do
      t <- S.jsonData
      let day = actionToMyday t
      did <- liftIO (runDb $ Sqlite.insert day)
      S.html "OK"
    S.notFound $ do
      S.text "404 no match found"
