{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div)
import Clay
import Data.Text.Lazy.IO as T

main :: IO ()
main = do T.writeFile "./static/css/customBootstrap.css" $ render customBootstrap
          return ()

customBootstrap :: Css
customBootstrap = do div # byId "loginbox"
                         ? do background grey
                              paddingBottom (px 20)
                              width (pct 80)
                     div # byId "loginhead"
                         ? width (pct 80)
                     button # byId "login-btn" ? width (px 80)
                     body ? paddingTop (px 70)
                     div # byClass "progress" ? margin (px 5) (px 5) (px 5) (px 5)
                     div # byClass "panel-body" ? paddingBottom (px 0)
                     div # byClass "panel-primary" |> div # byClass "panel-heading" |> a ? color white
