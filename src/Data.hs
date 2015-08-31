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
                             h4 ! class_ "panel-title pull-left"
                                $ do
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
editModal = div ! class_ "modal fade"
                ! A.style "display: none;"
                ! id "myModal"
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
                   $ do table ! class_ "table table-striped table-header-rotated"
                              $ do
                          thead $ tr
                              $ do
                                   th ""
                                   th ! class_ "rotate-45" $ div $ span
                                      $ do div "Location"
                                           div "from"
                                           div "to"
                                   th ! class_ "rotate-45" $ div $ span
                                      $ do div "Location Location Location Location"
                                           div "from    from    from    from    "
                                           div "to      to      to      to      "
                                   th ! class_ "rotate-45" $ div $ span
                                      $ do div "Location"
                                           div "from"
                                           div "to"
                          tbody $ do
                                tr $ do 
                                    th "Martin Heuschober"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                tr $ do 
                                    th "Martin Heuschober"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                tr $ do 
                                    th "Martin Heuschober"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                tr $ do 
                                    th "Martin Heuschober"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                tr $ do 
                                    th "Martin Heuschober"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                tr $ do 
                                    th "Martin Heuschober"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                tr $ do 
                                    th "Martin Heuschober"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                tr $ do 
                                    th "Martin Heuschober"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"
                                    td "[yes|maybe|no]"











