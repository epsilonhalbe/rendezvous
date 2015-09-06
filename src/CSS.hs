{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div, span)
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
                     th # byClass "rotated" ? do height (px 160)
                                                 whiteSpace nowrap
                     th # byClass "rotated" |> div ?
                         do transforms [ translate (px 25) (px 40)
                                       , rotate (deg 315)
                                       ]
                            width (px 30)
                     th # byClass "rotated" |> div |> span ?
                         borderBottom solid (px 1) (parse "#ccc")
                     th # byClass "rotate-45" ? do
                         height (px 200)
                         borderBottom solid (px 1) (parse "#ccc")
                         {-transforms [rotate (deg $ -90)]-}
                     label # byClass "btn" # byClass "active" ? textShadow (px 1) (px 1) (px 1) black
