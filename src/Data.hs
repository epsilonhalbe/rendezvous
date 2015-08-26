{-# LANGUAGE TemplateHaskell #-}
module Data where

import Control.Lens
import Snap.Snaplet.Auth (UserId)
import Data.Map.Lazy (Map)
import Data.Time
import Data.ByteString.Lazy

data Status = OK | TODO | CANCELLED | PENDING

data Confirmation = Accepted | Percent Int | Declined

data Coordinate = Coord { _location  :: String
                        , _startTime :: UTCTime
                        , _endTime   :: UTCTime
                        , _timeZone  :: TimeZone}

makeLenses ''Coordinate

data Attachment = Attachment { _attId      :: Int
                             , _attName    :: String
                             , _attMime    :: String
                             , _attContent :: ByteString}
makeLenses ''Attachment


data Rendezvous = Rdv  { _rdvId            :: Int
                       , _rdvTitle         :: String
                       , _rdvInitiator     :: UserId
                       , _rdvConfirms      :: Map UserId [(Coordinate, Confirmation)]
                       , _rdvComments      :: [(UserId, UTCTime, String)]
                       , _rdvAttachments   :: [Attachment]
                       , _rdvFix           :: Maybe Coordinate
                       }

makeLenses ''Rendezvous

-- TODO make lenses for participants,
-- TODO hash for identifying changes ??
