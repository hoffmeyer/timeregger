{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import qualified Controllers.Controller as Controller
import Model

main :: IO ()
main = do
  migrate
  scotty 3000 $
    do get "/" Controller.hello
       put "/login" Controller.login
       post "/users" Controller.createUser
       -- Authenticated routes
       get "/dates" $ Controller.authenticate Controller.getDates
       post "/dates" $ Controller.authenticate Controller.addDate
       put "/dates/:dateId" $ Controller.authenticate Controller.updateDate
       notFound $ text "404 no match found"
