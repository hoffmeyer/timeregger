{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Controllers.Controller as Controller
import           Model
import           Web.Scotty

main :: IO ()
main = do
  migrate
  scotty 3000 $
    do get "/" Controller.hello
       put "/login" Controller.login
       post "/users" Controller.createUser
       -- Authenticated routes
       delete "/users" $ Controller.authenticate Controller.deleteUser
       get "/dates" $ Controller.authenticate Controller.getDates
       get "/dates/:dateId" $ Controller.authenticate Controller.getDate
       post "/dates" $ Controller.authenticate Controller.addDate
       delete "/dates/:dateId" $ Controller.authenticate Controller.deleteDate
       put "/dates/:dateId" $ Controller.authenticate Controller.updateDate
       notFound $ text "404 no match found"
