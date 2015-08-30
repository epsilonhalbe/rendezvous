{-# LANGUAGE OverloadedStrings #-}
module Handler.Login
    (loginHandler) where

import Snap.Snaplet.Auth (loginUser, currentUser)
import Text.Blaze.Html5
import Snap.Blaze (blaze)

import Application

loginHandler :: AuthHandler ()
loginHandler = loginUser "username" "password" (Just "remember") onFailure onSuccess
  where
  onFailure authFail = blaze $ string $ "Error: " ++ show authFail
  onSuccess = do
    u <- currentUser
    case u of
      Just _ -> blaze $ docTypeHtml $ string $ show u
      Nothing -> blaze "Can't happen"
