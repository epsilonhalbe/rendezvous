{-# LANGUAGE OverloadedStrings #-}


module Handler.Root
    (rootHandler) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (form, label, title, span)
import Snap.Blaze (blaze)

import Prelude hiding (div, head, id, span)
import Application

import Html.Util

rootHandler :: AppHandler ()
rootHandler = blaze $ docTypeHtml $
--------------------------------------------------------------------------------
    do head $ do
         title "Rendezvous"
         meta ! name "viewport"
              ! content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
         stylesheet "static/node_modules/bootstrap/dist/css/bootstrap.css"
         stylesheet "static/node_modules/bootstrap/dist/css/bootstrap-theme.css"
         stylesheet "static/node_modules/font-awesome/css/font-awesome.css"
         stylesheet "static/css/customBootstrap.css"
--------------------------------------------------------------------------------
       body $ do
           div ! class_ "container"
               ! id "loginhead" $ do
               h1 $ do text "RENDEZVOUS "
                       small "(ˈrɒndɪvuː)"
               p "to meet at an agreed time and place"

           div ! class_"container"
               ! id "loginbox"
               $ do

               h2 "Login" ! id "login-h2"

               form ! method "POST"
                    ! id     "login"
                    ! action "/login" $ do
                   div ! id "input-div" $ do

                     withAddon "envelope" $
                         input ! class_ "form-control"
                               ! type_ "text"
                               ! name "username"
                               ! placeholder "email/username"

                     withAddon "lock" ! id "password-div" $
                         input ! class_ "form-control"
                               ! type_ "password"
                               ! name "password"
                               ! placeholder "password"

                   div ! id "remember-div" $ do
                       label ! id "remember-label"
                             $ do input ! id "remember"
                                        ! type_ "checkbox"
                                  " Remember Me"

                       div ! class_ "btn-group pull-right"
                           ! id "login-btn-div"
                           $ do
                           button "Login"
                               ! id "login-btn"
                               ! type_ "submit"
                               ! class_ "btn btn-primary"
                               ! action "submit"

                           button
                               ! type_ "button"
                               ! class_ "btn btn-primary dropdown-toggle"
                               ! dataToggle "dropdown"
                               ! customAttribute "aria-haspopup" "true"
                               ! customAttribute "aria-expanded" "false"
                               $ do
                                   span "" ! class_ "caret"
                                   span "Toggle Dropdown" ! class_ "sr-only"

                           ul ! class_ "dropdown-menu" $ do
                               li $ a "Login"    ! id "li-login"
                               li $ a "Register" ! id "li-register"

       script "" ! src "static/node_modules/jquery/dist/jquery.js"
       script "" ! src "static/node_modules/bootstrap/dist/js/bootstrap.js"
       script "" ! src "static/js/custom.js"

