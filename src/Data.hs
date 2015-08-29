{-# LANGUAGE TemplateHaskell #-}
module Data where

import Control.Lens
import Snap.Snaplet.Auth (UserId)
import Data.Map.Lazy as Map (Map, keys, lookup, foldl)
import Data.Time
import Data.ByteString.Lazy (ByteString)
import Data.List (intersect)
import Data.Maybe (fromMaybe)

data Status = OK | TODO | CANCELLED | PENDING

data Confirmation = Accepted | Percent Int | Declined

acceptable :: Confirmation -> Bool
acceptable Accepted = True
acceptable (Percent n) = n >= 66
acceptable _ = False

data Coordinate = Coord { _location  :: String
                        , _startTime :: UTCTime
                        , _endTime   :: UTCTime
                        } deriving (Eq)

makeLenses ''Coordinate

data Attachment = Attachment { _attId      :: Int
                             , _attName    :: String
                             , _attMime    :: String
                             , _attContent :: ByteString}
makeLenses ''Attachment

data Comment = Comment (UserId, UTCTime, String)

data Rendezvous = Rdv  { _rdvId            :: Int
                       , _rdvTitle         :: String
                       , _rdvInitiator     :: UserId
                       , _rdvConfirms      :: Map UserId [(Coordinate, Maybe Confirmation)]
                       , _rdvComments      :: [Comment]
                       , _rdvAttachments   :: [Attachment]
                       , _rdvFix           :: Maybe Coordinate
                       }

makeLenses ''Rendezvous

rdvParticipants :: Rendezvous -> [UserId]
rdvParticipants = keys . view rdvConfirms

rdvCoordinates :: Rendezvous -> [Coordinate]
rdvCoordinates = map fst . rdvCoordConfirm

rdvCoordConfirm :: Rendezvous -> [(Coordinate, Maybe Confirmation)]
rdvCoordConfirm r =  fromMaybe [] coords
                 where coords = (r^.rdvInitiator) `Map.lookup` (r^.rdvConfirms)

findConsensus :: Rendezvous -> [Coordinate]
findConsensus r = let coords = rdvCoordinates r
                      confirms = r ^. rdvConfirms
                  in Map.foldl aux coords confirms

    where aux :: [Coordinate] -> [(Coordinate, Maybe Confirmation)] -> [Coordinate]
          aux lst lstPair = lst ∩ map fst (filter (maybe False acceptable.snd) lstPair)

(∩) :: Eq a => [a] -> [a] -> [a]
(∩) = intersect






















