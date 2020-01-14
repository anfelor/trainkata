{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Function (on)
import Data.Ord
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)

data Reservation = Reservation
  { trainId :: String
  , bookingId :: String
  , seats :: [Seat]
  } deriving (Eq, Show)

data Seat = Seat
  { coach :: String
  , seatNumber :: Int
  } deriving (Eq, Show)

data ReservationRequest = ReservationRequest
  { reqTrainId :: String
  , seatCount :: Int
  } deriving (Eq, Show)

data TrainLimits = TrainLimits
  { minimumFreeSeats :: Int
  , coachMinimumFreeSeats :: Map String Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Reservation)
$(deriveJSON defaultOptions ''Seat)
$(deriveJSON defaultOptions ''ReservationRequest)

-- | Given a list of seats return the seats ordered by coaches
seatsByCoaches :: [Seat] -> [(String, [Seat])]
seatsByCoaches
  = map (\(s:ss) -> (coach s, s:ss))
  . groupBy ((==) `on` coach) 
  . sortBy (comparing coach)

-- | Given the minimum number of seats on our train
-- and a list of free seats on the train and a given
-- reservation request, return a reservation if possible.
makeReservation :: TrainLimits -> [Seat] -> ReservationRequest -> Maybe Reservation
makeReservation limits freeSeats request =
  if freeSeatsAfterReservation < minimumFreeSeats limits || seatCount request <= 0 
    then Nothing
    else case convenientCoaches of
      [] -> case possibleCoaches of
        [] -> Nothing
        xs -> resp xs
      xs -> resp xs
  where
    resp = Just 
         . Reservation (reqTrainId request) "" 
         . take (seatCount request) 
         . minimumBy (comparing length) 
         . map snd
    freeSeatsAfterReservation = length freeSeats - seatCount request
    sbc = seatsByCoaches freeSeats
    possibleCoaches = filter (\(_, ss) -> length ss >= seatCount request) sbc

    -- those that fulfill the 70% rule for coaches
    convenientCoaches = flip filter possibleCoaches $ \(c, ss) ->
      case Map.lookup c (coachMinimumFreeSeats limits) of
        Nothing -> False
        Just minFree -> length ss - seatCount request >= minFree

trainLimits :: [Seat] -> TrainLimits
trainLimits seats = TrainLimits
  (calcMin seats)
  (Map.fromList limits)
  where
    calcMin ss = ceiling $ 0.3 * fromIntegral (length ss)
    limits = map (\(c, ss) -> (c, calcMin ss)) (seatsByCoaches seats)

-- type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = return ()

-- startApp :: IO ()
-- startApp = run 8080 app

-- app :: Application
-- app = serve api server

-- api :: Proxy API
-- api = Proxy

-- server :: Server API
-- server = return users

-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
        -- , User 2 "Albert" "Einstein"
        -- ]