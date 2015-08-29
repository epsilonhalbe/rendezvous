module Test where

import Data

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

import qualified Data.Map.Lazy as M

rdv :: Rendezvous
rdv = Rdv { _rdvId = 1
          , _rdvTitle = "Rendezvous for testing purposes"
          , _rdvInitiator = 1
          , _rdvConfirms = M.fromList $ zip [1..5] (zip coords (repeat Nothing))
          , _rdvComments = [(1,mkTime "2015-07-12 12:00:00", "Wohooo a comment!")]
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
         ]
