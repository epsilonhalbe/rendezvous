{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div)
import Clay
import Data.Text.Lazy.IO as T

main :: IO ()
main = do T.writeFile "./static/css/customBootstrap.css" $ render customBootstrap

customBootstrap :: Css
customBootstrap = do div # byId "loginbox"
                         ? do background grey
                              paddingBottom (px 20)
                     button # byId "login-btn" ? width (px 80)
