{-# LANGUAGE OverloadedStrings #-}


module Handler.Dashboard
    (dashboardHandler) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (form, label, title, span)

import Snap.Blaze (blaze)

import Prelude hiding (div, head, id, span)

import Application
import Data
import Html.Util
import Testdata

dashboardHandler :: AuthHandler ()
dashboardHandler = blaze $ docTypeHtml $
--------------------------------------------------------------------------------
    do head $ do
         title "Rendezvous"
         meta ! name "viewport"
              ! content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
         stylesheet "static/node_modules/bootstrap/dist/css/bootstrap.css"
         stylesheet "static/node_modules/bootstrap/dist/css/bootstrap-theme.css"
         stylesheet "static/node_modules/font-awesome/css/font-awesome.css"
         stylesheet "static/css/customBootstrap.css"
         stylesheet "static/css/rotator.css"
--------------------------------------------------------------------------------
       body $ do
            nav ! class_ "navbar navbar-inverse navbar-fixed-top" $
                div ! class_ "container" $ do
                    div ! class_ "navbar-header" $ do
                        button ! type_ "button"
                               ! class_ "navbar-toggle collapsed"
                               ! dataToggle "collapse"
                               ! dataTarget "#navbar" $ do
                                   span ! class_ "sr-only" $ text "Toggle Navigation"
                                   span "" ! class_ "icon-bar"
                                   span "" ! class_ "icon-bar"
                                   span "" ! class_ "icon-bar"
                        div ! class_ "navbar-brand"
                            $ text "Rendezvous"
                    div ! id "navbar"
                        ! class_ "navbar-collapse collapse" $ do
                            ul ! class_ "nav navbar-nav" $ do
                                li ! class_ "active" $ a ! href "#" $ do
                                    faIcon "th-list"
                                    text " Dashboard"
                                li $ a ! href "#" $ do
                                       faIcon "plus-circle"
                                       text " Meeting"
                                li $ a ! href "#" $ do
                                       faIcon "calendar"
                                       text " Calendar"
                            ul ! class_ "nav navbar-nav navbar-right" $
                                li ! class_ "dropdown" $ do
                                   a ! href "#"
                                     ! class_ "dropdown-toggle"
                                     ! dataToggle "dropdown"
                                     {-! role "button"-}
                                     $ do faIcon "user"
                                          text " Martin Heuschober"
                                   ul ! class_ "dropdown-menu" $ do
                                      li $ a "Profile" ! href "#"
                                      li $ a "Log-out" ! href "#"

            div ! class_ "container-fluid" $
                div ! class_ "container" $ do
                    h1 "Dashboard"
                    div ! class_ "row" $ do
                        div ! class_ "col-md-6" $ do
                            h2 "Active RDV"
                            div ! class_ "panel-group"
                                ! id "active-rdv" $ do
                                rdv1
                                rdv3
                                yesMaybeNo
                                yesMaybeNo
                        div ! class_ "col-md-6" $ do
                            h2 "Past RDV"
                            div ! class_ "panel-group"
                                ! id "past-rdv" $
                                rdv2

            script "" ! src "static/node_modules/jquery/dist/jquery.js"
            script "" ! src "static/node_modules/bootstrap/dist/js/bootstrap.js"
            script "" ! src "static/js/custom.js"
            script "" ! src "static/js/dashboard.js"

rdv3 :: Html
rdv3 = toMarkup rdv

rdv1 :: Html
rdv1 = div ! class_ "panel panel-default panel-primary" $ do
           div ! class_ "panel-heading" $
               a ! dataToggle "collapse"
                 ! href "#collapseOne" $
                   h3 ! class_ "panel-title" $ do
                       toMarkup PENDING
                       text " "
                       text "Testtitel" ! class_ "rdv-title"
                       text " "
                       text "12.08.2015" ! class_ "rdv-date"
                       text " "
                       text "12:34" ! class_ "rdv-from-time"
                       text "-"
                       text "23:45" ! class_ "rdv-till-time"
                       text ", "
                       text "T-Center, +4d08" ! class_ "rdv-till-time"

           div ! id "collapseOne"
               ! class_ "panel-collapse collapse in" $
               div ! class_ "panel-body" $
                   ul ! class_ "list-unstyled" $
                       li $ do faIcon "user" ! A.style "color: brand-success;"
                               text " "
                               text "Martin Heuschober"
           div ! class_ "progress" $ do
               div ! class_ "progress-bar progress-bar-success"
                   ! A.style "width: 33%"
                   $ span "33% OK" ! class_ "sr-only"
               div ! class_ "progress-bar progress-bar-warning"
                   ! A.style "width: 10%"
                   $ span "10% Maybe" ! class_ "sr-only"
               div ! class_ "progress-bar progress-bar-danger"
                   ! A.style "width: 33%"
                   $ span "33% NOK" ! class_ "sr-only"



rdv2 :: Html
rdv2 =div ! class_ "panel panel-warning" $ do
          div ! class_ "panel-heading" $
              h3 ! class_ "panel-title" $ do
                  toMarkup TODO
                  text " "
                  text "Testtutel" ! class_ "rdv-title"
                  text " "
                  text "13.08.2015" ! class_ "rdv-date"
                  text " "
                  text "13:34" ! class_ "rdv-from-time"
                  text "-"
                  text "23:45" ! class_ "rdv-till-time"
                  text "T-Center, +4d08" ! class_ "rdv-till-time"
          div ! class_ "panel-body" $
              ul ! class_ "list-unstyled" $ do
                  li $ do faIcon "user" ! A.style "color: grey-dark;"
                          text " "
                          text "Martin Heuschober"
                  li $ do faIcon "user" ! A.style "color: grey;"
                          text " "
                          text "Hartin Meuschober"
                  li $ do faIcon "user" ! A.style "color: grey;"
                          text " "
                          text "Hertin Mauschober"


yesMaybeNo :: Html
yesMaybeNo = div ! class_ "btn-group input-group form-group"
                 ! A.style "margin-top: 10px;"
                 ! dataToggle "buttons"
                 $ do label ! class_ "btn btn-success"
                            $ do input ! type_ "radio"
                                       ! name "options"
                                       ! id "opt-ok"
                                       ! autocomplete "off"
                                 faIcon "check"
                      input ! id "input-maybe"
                            ! class_ "form-control"
                            ! placeholder "0-100"
                            ! A.style "padding: 0 0 0 5px; width: 48px; display: none;"
                      label ! class_ "btn btn-warning"
                            ! id "label-maybe"
                            $ do input ! type_ "radio"
                                       ! name "options"
                                       ! class_ "maybe-radio"
                                       ! id "opt-maybe"
                                       ! autocomplete "off"
                                 faIcon "question"
                      label ! class_ "btn btn-danger"
                            $ do input ! type_ "radio"
                                       ! name "options"
                                       ! id "opt-nok"
                                       ! autocomplete "off"
                                 faIcon "times"
