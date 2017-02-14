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
       put "/users" Controller.createUser
       -- Authenticated routes
       get "/hello/:name" $ Controller.authenticate Controller.helloName
       get "/dates" $ Controller.authenticate Controller.getDates
       put "/dates" $ Controller.authenticate Controller.addDate
       notFound $ text "404 no match found"
