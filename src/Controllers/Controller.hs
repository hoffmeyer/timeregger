{-# LANGUAGE OverloadedStrings #-}

module Controllers.Controller where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Crypto.PasswordStore as PW
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Monoid
import Model as M
import Web.Scotty
import Database.Persist
import Data.ByteString as BS hiding (pack, unpack)
import Data.ByteString.Char8 as BSC8 (pack, unpack)
import Data.Text.Lazy as LT

hello :: ActionM ()
hello = html "<h1>Hello World! Here we need to serve the actual app.</h1>"

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

deleteUser :: Entity User -> ActionM ()
deleteUser (Entity key user) = do
  liftIO $ M.deleteUser key
  html "OK"

getDates :: Entity User -> ActionM ()
getDates _ = do
  dates <- liftIO readDates
  json dates

-- TODO: verify thot you can only get your own dates
getDate :: Entity User -> ActionM ()
getDate _ = do
  dateId <- param "dateId"
  maybeDate <- liftIO $ readDate dateId
  case maybeDate of
    Just date -> json date
    Nothing -> raise $ LT.pack "Date not found"

addDate :: Entity User -> ActionM ()
addDate user = do
  t <- jsonData
  let day = actionToMyday t user
  did <- liftIO $ insertDates day
  html "OK"

updateDate :: Entity User -> ActionM ()
updateDate user = do
  t <- jsonData
  let day = actionToMyday t user
  dateId <- param "dateId"
  did <- liftIO $ M.updateDate day dateId
  html "OK"

-- TODO: verify thot you can only delete your own dates
deleteDate :: Entity User -> ActionM ()
deleteDate _ = do
  dateId <- param "dateId"
  liftIO $ M.deleteDate dateId
  html "OK"

fromRequest :: ActionM User
fromRequest = do
  uid <- param "user_id"
  token <- header "Authorization"
  case LT.words <$> token of
    Just ["Token", t] -> return $ User uid $ LT.unpack t
    _ -> raise $ LT.pack "Access denied"

authenticate :: (Entity User -> ActionM ()) -> ActionM ()
authenticate routeFor = do
  reqUser <- fromRequest
  user <- liftIO $ readUser $ userUsername reqUser
  case user of
    Just userEntity@(Entity _ foundUser) ->
      if userUsertoken reqUser == userUsertoken foundUser
        then routeFor userEntity
        else raise $ LT.pack "Acces denied"
    Nothing -> raise $ LT.pack "Acces denied"
