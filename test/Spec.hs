{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib -- (ReservationRequest(..), Seat(..), Reservation(..), initialTrain, )
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.QuickCheck
import Data.List ((\\))

main :: IO ()
main = hspec spec

initialTrain :: [Seat]
initialTrain =
  [ Seat str i
  | (str, num) <- [("A", 4), ("B", 8), ("C", 4)]
  , i <- [1..num]]

-- | An initial train with five seats reserved in B.
inconvenientTrain :: [Seat]
inconvenientTrain = filter (\(Seat s i) -> s /= "B" || i >= 6) initialTrain

applyRequest :: TrainLimits -> [Seat] -> Int -> [Seat]
applyRequest limits seats num = 
  case makeReservation limits seats (ReservationRequest "" num) of
    Nothing -> seats
    Just (Reservation _ _ reserved) -> seats \\ reserved

spec :: Spec
spec = do
  describe "makeReservation" $ do
    it "shouldn't book too much" $ do
      makeReservation 
        (trainLimits initialTrain) 
        initialTrain (ReservationRequest "" 12)
        `shouldBe` Nothing
    it "should reserve the right amount" $ do
      property $ \n ->
        case makeReservation 
          (trainLimits initialTrain) 
          initialTrain (ReservationRequest "" n) of
            Nothing -> True
            Just (Reservation _ _ seats) -> n == length seats
    it "shouldn't fill a coach over 70%" $ do
      ((coach . head . seats) <$> makeReservation
         (trainLimits initialTrain)
         inconvenientTrain (ReservationRequest "" 1))
         `shouldNotBe` Just "B"

-- spec = with (return app) $ do
    -- describe "GET /users" $ do
        -- it "responds with 200" $ do
            -- get "/users" `shouldRespondWith` 200
        -- it "responds with [User]" $ do
            -- let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
            -- get "/users" `shouldRespondWith` users
