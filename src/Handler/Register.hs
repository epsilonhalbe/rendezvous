{-# LANGUAGE OverloadedStrings #-}
module Handler.Register
    (registerHandler) where

import Snap.Core (getParam)
import Snap.Snaplet.Auth (registerUser)

import Text.Blaze.Html5
import Snap.Blaze (blaze)

import Application

registerHandler :: AuthHandler ()
registerHandler = do
    pw  <- getParam "password"
    cpw <- getParam "cpassword"
    if pw == cpw
      then do authUser <- registerUser "username" "password"
              blaze $ docTypeHtml $ string $ show authUser
      else blaze $ docTypeHtml $ text "passwords do not match"

