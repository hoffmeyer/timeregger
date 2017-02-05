{-# LANGUAGE OverloadedStrings #-}

module Controllers.Controller where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Crypto.PasswordStore as PW
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Monoid
import Model
import Web.Scotty
import Database.Persist
import Data.ByteString hiding (pack, unpack)
import Data.ByteString.Char8 (pack, unpack)

hello :: ActionM ()
hello = html "Hello World!"

helloName :: ActionM ()
helloName = do
  name <- param "name"
  text ("hello " <> name <> "!")

createUser :: ActionM ()
createUser = do
  user <- jsonData
  uid <- liftIO $ insertUser user
  html "OK"

getDates :: ActionM ()
getDates = do
  todos <- liftIO readDates
  json todos

addDate :: ActionM ()
addDate = do
  t <- jsonData
  let day = actionToMyday t
  did <- liftIO $ insertDates day
  html "OK"

login :: ByteString -> ByteString -> IO Bool
login username password = do
  user <- liftIO $ readUser $ unpack username
  case user of
    Just (Entity _ foundUser) -> return PW.verifyPassword password (pack $ userUserToken foundUser)
    Nothing -> return False
