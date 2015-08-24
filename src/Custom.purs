module Main where

import Prelude hiding (div, id, append)

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Bind ((=<<))
import Data.Either
import Data.Monoid
import Data.String (toLower)

import qualified Data.Foreign as F
import DOM (DOM())
import Text.Smolder.HTML (Html(), div, label, input, p, i)
import Text.Smolder.HTML.Attributes ( id
                                    , placeholder
                                    , type'
                                    , className)
import Text.Smolder.Markup (Markup(), text, (!))
import Text.Smolder.Renderer.String (render)
import qualified Control.Monad.Eff.JQuery as J

main = do J.select "li" >>= J.on "click" fun


fun :: forall eff. J.JQueryEvent -> J.JQuery -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
fun _ _ = do content <- (F.readString <$> (J.getValue =<< J.select "this"))
             case content of
                  Right "Register" -> do J.select "#remember-label" >>= J.remove
                                         jqueryDiv <- J.create cpasswordDiv
                                         J.select "#confirm-div" >>= J.append jqueryDiv
                                         aux "Register"

                  Right "Login" -> do J.select "#confirm-div" >>= J.remove
                                      jqueryDiv <- J.create rememberDiv
                                      J.select "#remember-div" >>= J.append jqueryDiv
                                      aux "Login"
                  _ -> return unit

    where aux :: forall eff. String -> Eff (dom :: DOM | eff) Unit
          aux str = do J.select "#login-btn" >>= J.setValue str
                       J.select "#login" >>= J.setAttr "action" ("/" ++ toLower str)
                       J.select "#login-h2" >>= J.setValue str
                       return unit



rememberDiv :: String
rememberDiv = render $ div ! id "remember-div" $ do
                  label ! id "remember-label"
                        $ do input ! id "remember"
                                   ! type' "checkbox"
                             text "Remember Me"

cpasswordDiv :: String
cpasswordDiv = render $ withAddon "lock" ! id "confirm-div" $
                   input ! className "form-control"
                         ! type' "password"
                         ! placeholder "retype password"

withAddon :: String -> Markup -> Markup
withAddon addon htmlInput =
        div ! className "form-group" $
            div ! className"input-group" $ do
                p (faIcon addon) ! className "input-group-addon"
                htmlInput

faIcon :: String -> Markup
-- | inserts a fontawsome icon (fa) with fixed width (fa-fw)
-- see http://fontawesome.io/icons/ for a complete set of icons
faIcon str = i mempty ! className ("fa fa-fw fa-" ++ str)
