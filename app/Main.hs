{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson         hiding (json)
import           Data.Monoid
import           Data.Time.Calendar
import           Data.Time.Clock
import           Lib
import           Web.Scotty

data MyDay = MyDay { start :: UTCTime, end :: UTCTime } deriving Show

today :: MyDay
today = MyDay { start = UTCTime (fromGregorian 2011 12 16) (fromIntegral $ 12 * 3600), end = UTCTime (fromGregorian 2011 12 16) (fromIntegral $ 11 * 3600) }

instance ToJSON MyDay where
  toJSON d = object [
    "start" .= start d,
    "end" .= end d ]

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html "Hello World!"
  get "/hello/:name" $ do
    name <- param "name"
    text ("hello " <> name <> "!")
  get "/date" $ do
    json today
  notFound $ do
    text "404 no match found"
