{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid
import           Model
import qualified Web.Scotty             as S

main :: IO ()
main = do
  migrate
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
      did <- liftIO $ insertDates day
      S.html "OK"
    S.notFound $ do
      S.text "404 no match found"
