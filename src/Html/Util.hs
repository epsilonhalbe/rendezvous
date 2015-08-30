{-# LANGUAGE OverloadedStrings #-}

module Html.Util where

import Data.Monoid ((<>))
import Prelude hiding (div, id)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes as A hiding (form, label, title, span)


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
        div ! class_ "form-group" $
            div ! class_"input-group" $ do
                p ! class_ "input-group-addon" $ faIcon addon
                htmlInput

dataToggle :: AttributeValue -> Attribute
dataToggle = dataAttribute "toggle"

dataTarget :: AttributeValue -> Attribute
dataTarget = dataAttribute "target"
