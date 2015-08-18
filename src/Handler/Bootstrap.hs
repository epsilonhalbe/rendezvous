{-# LANGUAGE OverloadedStrings #-}

module Handler.Bootstrap where


import Data.Monoid ((<>))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (form, label, title, span)
import Snap.Blaze (blaze)

import Prelude hiding (div, head, id, span)
import Application

bootstrapHandler :: AppHandler ()
bootstrapHandler = blaze $ docTypeHtml $
--------------------------------------------------------------------------------
    do head $ do
        title "Rendezvous"
        meta ! name "viewport"
             ! content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
        stylesheet "static/bootstrap/css/bootstrap.css"
        stylesheet "static/bootstrap/css/bootstrap-theme.css"
        stylesheet "static/font-awesome/css/font-awesome.css"
--------------------------------------------------------------------------------
        body $ do
            div ! class_"container" $ do
                h1 $ do text "RENDEZVOUS "
                        small "(ˈrɒndɪvuː)"
                p "to meet at an agreed time and place"
            div ! class_"container"
                ! A.style "background: grey;"
                $ do

                h2 "Login"

                form ! method "POST"
                     ! id     "login"
                     ! action "/login" $ do

                    withAddon "envelope" $
                        input ! class_ "form-control"
                              ! type_ "text"
                              ! placeholder "email/username"

                    withAddon "lock" $
                        input ! class_ "form-control"
                              ! type_ "password"
                              ! placeholder "password"

                    div ! class_ "checkbox" $ do
                        label $ do input ! type_ "checkbox"
                                   "Remember Me"

                        div ! class_ "btn-group pull-right" $ do
                            button "Login"
                                ! type_ "button"
                                ! class_ "btn btn-primary"

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
                                li $ a "Login" ! href "#"
                                li $ a "Register" ! href "#"

        script "" ! src "static/js/jquery-2.1.4.js"
        script "" ! src "static/bootstrap/js/bootstrap.js"

stylesheet :: AttributeValue -> Html
-- | shorthand helper for inserting a css-stylesheet
stylesheet url = link ! rel "stylesheet"
                      ! type_ "text/css"
                      ! href url

faIcon :: AttributeValue -> Html
-- | inserts a fontawsome icon (fa) with fixed width (fa-fw)
-- see http://fontawesome.io/icons/ for a complete set of icons
faIcon str = i "" ! class_ ("fa fa-fw fa-" <> str)

withAddon :: AttributeValue -> Html -> Html
withAddon addon htmlInput =
        div ! class_ "form-group" $ do
            div ! class_"input-group" $ do
                p ! class_ "input-group-addon" $ faIcon addon
                htmlInput

dataToggle :: AttributeValue -> Attribute
dataToggle = dataAttribute "toggle"
