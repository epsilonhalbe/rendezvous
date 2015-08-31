{-# LANGUAGE OverloadedStrings #-}

module Testdata where

import Data

import Data.Time.Clock
import Data.Time.Format
import Snap.Snaplet.Auth (UserId(..))

import qualified Data.Map.Lazy as M

rdv :: Rendezvous
rdv = Rdv { _rdvId = 1
          , _rdvTitle = "Rendezvous for testing purposes"
          , _rdvInitiator = UserId "Martin Heuschober"
          , _rdvConfirms = M.fromList $ zipWith confirms  [UserId "Martin Heuschober"
                                                          ,UserId "Hartin Meuschober"
                                                          ,UserId "Hertin Mauschober"
                                                          ,UserId "Heutin Marschober"
                                                          ,UserId "Heusin Martchober"
                                                          ]
                                                          [1..]
          , _rdvComments = [Comment (UserId "")  (mkTime "2015-07-12 12:00:00") "Wohooo a comment!"]
          , _rdvAttachments = []
          {-, _rdvFix = Nothing-}
          , _rdvFix = Just (head coords)

          }

mkTime :: String -> UTCTime
mkTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S"

coords :: [Coordinate]
coords = [Coord { _location = "Vienna"
                , _startTime = mkTime "2015-08-12 12:00:00"
                , _endTime = mkTime "2015-08-12 13:00:00"
                }
         ,Coord { _location = "Vienna"
                , _startTime = mkTime "2015-08-19 13:00:00"
                , _endTime = mkTime "2015-08-19 14:00:00"
                }
         ,Coord { _location = "Vienna"
                , _startTime = mkTime "2015-08-26 13:00:00"
                , _endTime = mkTime "2015-08-26 14:00:00"
                }
         ]

confirms :: UserId -> Int -> (UserId, [(Coordinate, Maybe Confirmation)])
confirms uid n = (uid, zip coords (accept n))
    where accept 1 = [Just Accepted     , Just Accepted     , Just Accepted] ++ repeat Nothing
          accept 2 = [Just Accepted     , Just $ Percent 90 , Just Declined] ++ repeat Nothing
          accept 3 = [Just $ Percent 70 , Just Declined     , Nothing] ++ repeat Nothing
          accept 4 = [Just Declined     , Just $ Percent 50 , Nothing] ++ repeat Nothing
          accept 5 = [Nothing           , Nothing           , Nothing] ++ repeat Nothing
          accept _ = repeat Nothing
