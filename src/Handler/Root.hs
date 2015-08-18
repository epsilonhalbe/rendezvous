{-# LANGUAGE OverloadedStrings #-}

module Handler.Root where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (form, label, title)
import Snap.Blaze (blaze)

import Prelude hiding (div, head, id)
import Application

rootHandler :: AppHandler ()
rootHandler = blaze $ docTypeHtml $
--------------------------------------------------------------------------------
    do head $ do title "Rdv"
                 link ! rel "stylesheet"
                      ! type_ "text/css"
                      ! href "/static/css/style.css"
--------------------------------------------------------------------------------
       body $ do
         h1 "«Rendezvous»"
         div ! class_"loginbox" $ do
           h2 "Login"
           form ! method "POST"
                ! id     "login"
                ! action "/login" $ do
             p $ input' "username" "text"
             p $ input' "password" "password"
             label ! for "username" $ "Remember me: "
             checkbox' "remember"

             a square ! onclick "document.getElementById('login').submit()"
                      ! href "#"
         {-div ! class_ "loginbox" $ h2 $-}
             {-a "Register" -- ! onclick "alert('mister register!')"-}
                            {-! href "/register"-}

         div ! class_"loginbox" $ do
           h2 "Register"
           form ! method "POST"
                ! id     "register"
                ! action "/register" $ do
             p $ input' "username" "text"
             p $ input' "password"  "password"
             p $ input' "cpassword" "password"

             a square ! onclick "document.getElementById('register').submit()"
                      ! href "#"

  where
    input' str t = input ! type_ t
                         ! name  str
                         ! id str
                         ! placeholder str

    checkbox' str = input ! type_ "checkbox"
                          ! name str
                          ! id   str
                          ! value ""
    square = div ! class_ "btn" $ text ""
