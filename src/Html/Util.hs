{-# LANGUAGE OverloadedStrings #-}


module Html.Util where

import Data.Monoid ((<>))
import Prelude hiding (div, id)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes as A hiding (form, label, title, span)
import Data.String (fromString)
import Data.Char (toLower)


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

dataDismiss :: AttributeValue -> Attribute
dataDismiss = dataAttribute "dismiss"

dataPlacement :: AttributeValue -> Attribute
dataPlacement = dataAttribute "placement"

dataHtml :: Bool -> Attribute
dataHtml = dataAttribute "dismiss" . fromString . map toLower . show

dataContent :: AttributeValue -> Attribute
dataContent = dataAttribute "content"

ariaLabelledBy :: AttributeValue -> Attribute
ariaLabelledBy = ariaAttribute "labelledby"

ariaLabel :: AttributeValue -> Attribute
ariaLabel = ariaAttribute "label"

ariaHidden :: AttributeValue -> Attribute
ariaHidden = ariaAttribute "hidden"

ariaAttribute :: String -> AttributeValue -> Attribute
ariaAttribute aria = customAttribute $ fromString ("aria-"<>aria)
