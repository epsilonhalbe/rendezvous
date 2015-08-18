{-# LANGUAGE OverloadedStrings #-}
module Handler.Login where

import Snap.Snaplet.Auth (registerUser)
import Text.Blaze.Html5
{-import Text.Blaze.Html5.Attributes hiding (form, label, title)-}
import Snap.Blaze (blaze)

import Application

loginHandler :: AuthHandler ()
loginHandler = do loginUserName <- registerUser "username" "password"
                  blaze $ docTypeHtml $ string $ show loginUserName

