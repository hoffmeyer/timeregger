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
import           Control.Monad                (forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   hiding (json)
import           Data.Monoid
import           Data.Time                    (UTCTime, getCurrentTime)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Lib
import qualified Web.Scotty                   as S


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MyDay json
    start UTCTime
    minutes Int
    deriving Show
|]

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn

readDates :: IO [Entity MyDay]
readDates = runDb $ selectList [] [LimitTo 10]

today :: MyDay
today = MyDay (UTCTime (fromGregorian 2011 12 16) (fromIntegral $ 12 * 3600)) 450

main :: IO ()
main = S.scotty 3000 $ do
  S.get "/" $ do
    S.html "Hello World!"
  S.get "/hello/:name" $ do
    name <- S.param "name"
    S.text ("hello " <> name <> "!")
  S.get "/date" $ do
    S.json today
  S.notFound $ do
    S.text "404 no match found"
