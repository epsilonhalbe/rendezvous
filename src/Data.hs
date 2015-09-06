{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data where

import Control.Lens
import Data.Monoid ((<>))
import Data.ByteString.Lazy (ByteString)
import Data.List (intersect, (\\))
import Data.Map.Lazy as Map (Map, keys, lookup, foldl, toList)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.String (IsString(..))
import Snap.Snaplet.Auth (UserId(..))
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A hiding (form, label, title, span)

import Prelude hiding (div, head, id, span)

import           Html.Util

data Status = OK | TODO | CANCELLED | PENDING

instance ToMarkup Status where
  toMarkup OK        = faIcon "check-circle"       ! A.style "color: green;"
  toMarkup CANCELLED = faIcon "times-circle"       ! A.style "color: red;"
  toMarkup TODO      = faIcon "exclamation-circle" ! A.style "color: orange;"
  toMarkup PENDING   = faIcon "circle"             ! A.style "color: grey;"

data Confirmation = Accepted | Percent Int | Declined
  deriving (Eq)

instance Show Confirmation where
    show Accepted = "OK"
    show (Percent n) = show n ++ "%"
    show Declined = "NOK"

instance Ord Confirmation where
    compare Accepted Accepted = EQ
    compare Accepted _ = GT
    compare _ Accepted = LT
    compare (Percent n) (Percent m) = compare n m
    compare (Percent _) _ = GT
    compare _ (Percent _) = LT
    compare _ _ = EQ

acceptable :: Confirmation -> Bool
acceptable Accepted = True
acceptable (Percent n) = n >= 66
acceptable _ = False

data Coordinate = Coord { _location  :: String
                        , _startTime :: UTCTime
                        , _endTime   :: UTCTime
                        } deriving (Eq)

makeLenses ''Coordinate

showDateTime :: UTCTime -> String
showDateTime showdt = unwords [ showDay . (\(_,_,rd) -> rd) . toGregorian . utctDay $ showdt
                             , formatTime defaultTimeLocale "%b %Y, %H:%M" showdt ]
                where showDay d = show d ++ suffix d
                      suffix 1 = "st"
                      suffix 2 = "nd"
                      suffix 3 = "rd"
                      suffix d | 4 <= d && d <= 13 = "th"
                               | otherwise = suffix (d `rem` 10)

instance Show Coordinate where
    show (Coord l meetStart meetEnd) =
        unwords [ l ++ ","
                , showDateTime meetStart
                , "-"
                , if utctDay meetStart == utctDay meetEnd
                     then formatTime defaultTimeLocale "%H:%M" meetEnd
                     else showDateTime meetEnd
                ]

instance ToMarkup Coordinate where
    toMarkup d = H.string $ show d

data Attachment = Attachment { _attId      :: Int
                             , _attName    :: String
                             , _attMime    :: String
                             , _attContent :: ByteString}
makeLenses ''Attachment

data Comment = Comment { _commentWho  :: UserId
                       , _commentWhen :: UTCTime
                       , _commentWhat :: String}

instance Show Comment where
    show (Comment who when what) = unwords [ show who ++ ":"
                                           , showDateTime when
                                           , what
                                           ]

data Rendezvous = Rdv  { _rdvId            :: Int
                       , _rdvTitle         :: String
                       , _rdvInitiator     :: UserId
                       , _rdvConfirms      :: Map UserId [(Coordinate, Maybe Confirmation)]
                       , _rdvComments      :: [Comment]
                       , _rdvAttachments   :: [Attachment]
                       , _rdvFix           :: Maybe Coordinate
                       }

makeLenses ''Rendezvous

instance ToMarkup Rendezvous where
    toMarkup Rdv{..} =
        panelDiv $ do titleDiv
                      participantDiv
                      progressDiv
                      editModal
        where ifFixed =  maybe "default" (const "primary") _rdvFix
              panelDiv = div ! class_ ("panel panel-" <> ifFixed)
              titleDiv = div ! class_ "panel-heading clearfix" $ do
                             h4 ! class_ "panel-title pull-left" $
                                a ! dataToggle "collapse"
                                  ! href (fromString $ "#" ++ show _rdvId ) $ do
                                  strong (string _rdvTitle ! class_ "rdv-title")
                                  br
                                  string $ maybe "" (('\n' :).show) _rdvFix
                             div ! class_ "btn-group pull-right" $
                                 a ! class_ ("btn btn-" <> ifFixed <> " btn-small")
                                   ! href "#"
                                   ! dataTarget "#myModal"
                                   ! dataToggle "modal"
                                   $ faIcon "eye"

              participantDiv =  div ! (id . fromString $ show _rdvId)
                                    ! class_ "panel-collapse collapse" $
                                    div ! class_ "panel-body" $
                                        ul ! class_ "list-unstyled" $ do
                                            li $ do faIcon "user"
                                                    text " "
                                                    text $ unUid _rdvInitiator
                                            mapM_ participant (keys _rdvConfirms \\ [_rdvInitiator])

              progressDiv = div ! class_ "progress" $
                              case _rdvFix of
                                Nothing -> let lenFilt = length [() | (_,lst) <- toList _rdvConfirms
                                                                    , not . null $ [()| (_,Just _) <- lst]]
                                               lenAll  = length $ toList _rdvConfirms
                                               pct = show $ (100 * lenFilt) `quot` lenAll
                                           in div ! class_ "progress-bar"
                                                  ! A.style (fromString $ "width: "<> pct <>"%")
                                                  $ span (fromString $ pct <> "%") ! class_ "sr-only"
                                Just fix -> do
                                    let cc = [confirm | (_,lst) <- toList _rdvConfirms
                                                      , (coord,confirm) <- lst
                                                      ,  coord == fix]
                                        len = length cc
                                        success  = show $ (100 * length [()| Just Accepted    <- cc]) `quot` len
                                        percent  = show $ (100 * length [()| Just (Percent _) <- cc]) `quot` len
                                        declined = show $ (100 * length [()| Just Declined    <- cc]) `quot` len
                                    progressbar "success" success
                                    progressbar "warning" percent
                                    progressbar "danger"  declined

progressbar :: String -> String -> Html
progressbar str pct = div ! class_  (fromString $ "progress-bar progress-bar-" <> str)
                          ! A.style (fromString $ "width: " <> pct <>"%")
                          $ span    (fromString $              pct <>"%") ! class_ "sr-only"


participant :: UserId -> Html
participant username = li $ do faIcon "user" ! A.style "color: grey;"
                               text " "
                               text $ unUid username

rdvParticipants :: Rendezvous -> [UserId]
rdvParticipants = keys . view rdvConfirms

rdvCoordinates :: Rendezvous -> [Coordinate]
rdvCoordinates = map fst . rdvCoordConfirm

rdvCoordConfirm :: Rendezvous -> [(Coordinate, Maybe Confirmation)]
rdvCoordConfirm r =  fromMaybe [] _coords
                 where _coords = (r^.rdvInitiator) `Map.lookup` (r^.rdvConfirms)

findConsensus :: Rendezvous -> [Coordinate]
findConsensus r = let _coords = rdvCoordinates r
                      confirms = r ^. rdvConfirms
                  in Map.foldl aux _coords confirms

    where aux :: [Coordinate] -> [(Coordinate, Maybe Confirmation)] -> [Coordinate]
          aux lst lstPair = lst ∩ map fst (filter (maybe False acceptable.snd) lstPair)

(∩) :: Eq a => [a] -> [a] -> [a]
(∩) = intersect

editModal ::  Html
editModal = div ! id "myModal"
                {-! A.style "display: none;"-}
                {-! class_ "modal fade"-}
                ! A.style "display: block;"
                ! class_ "modal fade in"
                ! tabindex "-1"
                ! ariaLabelledBy "myModalLabel" $
                div ! class_ "modal-dialog" $
                  div ! class_ "modal-content" $ do
                    div ! class_ "modal-header" $ do
                      button ! type_ "button"
                             ! class_ "close"
                             ! dataDismiss "modal"
                             ! ariaLabel "Close" $
                             span "×" ! ariaHidden "true"
                      h4 ! class_ "modal-title"
                         ! id "myModalLabel"
                         $ "Testtitel für das Modal"
--                      small "Martin Heuschober (initiator)"
                    div ! class_ "modal-body" $
                        modalContent
                    div ! class_ "modal-footer" $ do
                      button ! type_ "button"
                             ! class_ "btn btn-default"
                             ! dataDismiss "modal"
                             $ "Close"
                      button ! type_ "button"
                             ! class_ "btn btn-primary"
                             $ "Save changes"

modalContent ::  Html
modalContent = div ! class_ "scrollable-table table-responsive"
                   {-$ table ! class_ "table table-striped table-header-rotated"-}
                   $ table ! class_ "table table-striped" $
                       do thead $ tr
                              $ do th ""
                                   th ! A.style "padding: 0 0 0 0;"
                                      $ test
                                   th ! A.style "padding: 0 0 0 0;"
                                      $ test
                                   th ! A.style "padding: 0 0 0 0;"
                                      $ test
                          tbody $ do
                                tr $ do
                                    th ! A.style "vertical-align: middle;" $ "Martin Heuschober"
                                    mconcat $ replicate 3 yesMaybeNo'
                                tr $ do
                                    th "Hartin Meuschober"
                                    yesMaybeNo'
                                    yesMaybeNo'
                                    yesMaybeNo'
    where yesMaybeNo' = td yesMaybeNo

yesMaybeNo :: Html
yesMaybeNo = div ! class_ "btn-group input-group form-group pull-right"
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

test :: Html
test = div ! class_ "form-group"
           ! A.style "margin-bottom: 0px;" $ do
           ul ! class_ "list-group"
              ! A.style "margin-bottom: 0px;"
              $ li ! class_ "list-group-item"
                   ! A.style "background-color: #eee;"
                   $ do div "T-Center"
                        small $ do text "12.08.2015 13"
                                   sup "00"
                                   text "-14"
                                   sup "00"
           div ! class_"input-group" $ do
               a ! class_ "input-group-addon"
                 $ faIcon "globe" ! href "https://www.google.at/maps/place/T-Mobile/@48.1867866,16.4030867,17z/data=!3m1!4b1!4m2!3m1!1s0x476d0758bd8a5b27:0xc82262fbd030dbd0"
               ul ! class_ "list-group"
                  ! A.style "margin-bottom: 0px;"
                  $ do li ! class_ "list-group-item"
                          ! A.style (  "padding-bottom: 0px;"
                                    <> "padding-top: 0px;")
                          $ h4 "T-Center"
                       li ! class_ "list-group-item" $ do
                          div "Room 4c08"
                          div "Rennweg 97-99"
                          div "A-1030"
                          div "Vienna"
                          div "Austria"
           div ! class_"input-group" $ do
               span ! class_ "input-group-addon"
                    $ faIcon "clock-o" ! href "https://www.google.at/maps/place/T-Mobile/@48.1867866,16.4030867,17z/data=!3m1!4b1!4m2!3m1!1s0x476d0758bd8a5b27:0xc82262fbd030dbd0"
               ul ! class_ "list-group"
                  ! A.style "margin-bottom: 0px;"
                  $ do li ! class_ "list-group-item"
                          ! A.style "padding-bottom: 5px;"
                          $ text "12" <> sup "th" <> text " Aug. 2015, 13" <> sup "00"
                       li ! class_ "list-group-item"
                          ! A.style "padding-top: 5px;"
                          $ text "12" <> sup "th" <> text " Aug. 2015, 14" <> sup "00"

