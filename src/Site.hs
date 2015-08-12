{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative()
import           Control.Monad (forM_)
import           Data.ByteString (ByteString)
import           Data.Text()
import           Snap.Core()
import           Snap.Blaze (blaze)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist (heistInit)
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist()
{-import qualified Heist.Interpreted as I-}
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("/",         testHandler)
         ,("/login",    loginHandler)
         ,("/static",   serveDirectory "static")
         ]

testHandler :: Handler App App ()
testHandler = blaze $ H.docTypeHtml $
    do H.head $ do H.title "SnaptestBlaze"
                   H.link H.! A.rel "stylesheet"
                          H.! A.type_ "text/css"
                          H.! A.href "/static/style.css"

       H.body $ do H.p "Blaze makes Html"
                   H.ul $ forM_ [1..10::Int] (H.li . H.toHtml)

loginHandler :: Handler App App ()
loginHandler = blaze $ H.docTypeHtml $
--------------------------------------------------------------------------------
    do H.head $ do H.title "Rdv"
                   H.link H.! A.rel "stylesheet"
                          H.! A.type_ "text/css"
                          H.! A.href "/static/style.css"
--------------------------------------------------------------------------------
       H.body $ do
         H.h1 "«Rendezvous»"
         H.div H.! A.class_"loginbox" $ do
           H.h2 "Login"
           H.form H.! A.method "POST"
                  H.! A.id     "login"
                  H.! A.action "login" $ do
             H.p $ input' "username" "text"
             H.p $ input' "password" "password"
             H.label H.! A.for "username" $ "Remember me: "
             checkbox' "remember"

             H.a square H.! A.onclick "document.getElementById('login').submit()"
                        H.! A.href "#"
         H.div H.! A.class_ "loginbox" $ H.h2 $
             H.a "Register" H.! A.onclick "alert('mister register!')"
                            H.! A.href "#"


  where
    input' str t = H.input H.! A.type_ t
                         H.! A.name  str
                         H.! A.id str
                         H.! A.placeholder str

    checkbox' str = H.input H.! A.type_ "checkbox"
                            H.! A.name str
                            H.! A.id   str
                            H.! A.value ""
    square = H.div H.! A.class_ "btn" $ H.text ""




------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a

