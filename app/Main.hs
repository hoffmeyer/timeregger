{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai
import qualified Controllers.Controller as Controller
import Model
import Network.Wai.Middleware.HttpAuth

main :: IO ()
main = do
  migrate
  scotty 3000 $
    do middleware $
         basicAuth
           authorize
           ("timeregger"
            { authIsProtected = authProtected
            } :: AuthSettings)
       get "/" Controller.hello
       get "/hello/:name" Controller.helloName
       put "/users" Controller.createUser
       get "/dates" Controller.getDates
       put "/dates" Controller.addDate
       notFound $ text "404 no match found"

authProtected :: Request -> IO Bool
authProtected r =
  pure $
  case pathInfo r of
    [] -> False
    x:xs -> x /= "users"

authorize :: CheckCreds
authorize = Controller.login
