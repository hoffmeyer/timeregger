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
import Data.ByteString as BS hiding (pack, unpack)
import Data.ByteString.Char8 as BSC8 (pack, unpack)
import Data.Text.Lazy as LT

hello :: ActionM ()
hello = html "Hello World!"

login :: ActionM ()
login = do
  newUser <- jsonData
  user <- liftIO $ readUser $ newUsername newUser
  case user of
    Just (Entity _ foundUser) ->
      if PW.verifyPassword (BSC8.pack $ newPassword newUser) (BSC8.pack $ userUsertoken foundUser)
        then json user
        else raise $ LT.pack "Invalid password"
    Nothing -> raise $ LT.pack "Invalid username"

createUser :: ActionM ()
createUser = do
  user <- jsonData
  uid <- liftIO $ insertUser user
  html "OK"

getDates :: User -> ActionM ()
getDates _ = do
  todos <- liftIO readDates
  json todos

addDate :: User -> ActionM ()
addDate _ = do
  t <- jsonData
  let day = actionToMyday t
  did <- liftIO $ insertDates day
  html "OK"

updateDate :: User -> ActionM ()
helloName _ = do
  name <- param "dateId"
  text ("the date id was:  " <> dateId <> "!")


fromRequest :: ActionM User
fromRequest = do
  uid <- param "user_id"
  token <- header "Authorization"
  case LT.words <$> token of
    Just ["Token", t] -> return $ User uid $ LT.unpack t
    _ -> raise $ LT.pack "Access denied"

authenticate :: (User -> ActionM ()) -> ActionM ()
authenticate routeFor = do
  reqUser <- fromRequest
  user <- liftIO $ readUser $ userUsername reqUser
  case user of
    Just (Entity _ foundUser) ->
      if userUsertoken reqUser == userUsertoken foundUser
        then routeFor foundUser
        else raise $ LT.pack "Acces denied"
    Nothing -> raise $ LT.pack "Acces denied"
