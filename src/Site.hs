{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Control.Concurrent
import           Control.Monad.Trans (liftIO)
import           Control.Lens

import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist (heistInit)
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe

------------------------------------------------------------------------------
import           Application
import           Handler


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("/"          , rootHandler)
         ,("/login"     , with auth $ loginHandler)
         ,("/register"  , with auth $ registerHandler)

         ,("/static"    , serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db sqliteInit
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    {-let c = sqliteConn $ d ^# snapletValue-}
    {-liftIO $ withMVar c $ \conn -> Db.createTables conn-}
    addAuthSplices h auth
    return $ App h s d a

