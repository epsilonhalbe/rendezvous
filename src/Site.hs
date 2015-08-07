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
         ,("",          serveDirectory "static")
         ]

testHandler :: Handler App App ()
testHandler = blaze $ H.docTypeHtml $
    do H.head $ H.title "SnaptestBlaze"
       H.body $ do H.p "Blaze makes Html"
                   H.ul $ forM_ [1..21::Int] (H.li . H.toHtml)

loginHandler :: Handler App App ()
loginHandler = blaze $ H.docTypeHtml $
    do H.head $ H.title "Rdv"
       H.body $ do
         H.div H.! A.id "loginbox" $ do
           H.h2 "Login"
           H.form H.! A.method "POST"
                  H.! A.action "login" $ do
             H.label H.! A.for "login" $ "Username: "
             H.input H.! A.type_ "text"
                     H.! A.name "login"
                     H.! A.id "login"
                     H.! A.value ""
                     H.! A.placeholder "username"
             H.hr
             H.label H.! A.for "password" $ "Password: "
             H.input H.! A.type_ "text"
                     H.! A.name "password"
                     H.! A.id "password"
                     H.! A.value ""
                     H.! A.placeholder "password"
             H.br
             H.label H.! A.for "login" $ "Remember me: "
             H.input H.! A.type_ "checkbox"
                     H.! A.name "remember"
                     H.! A.id   "remember"
                     H.! A.value ""
             H.input H.! A.type_ "submit"
                     H.! A.value "Login"


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

