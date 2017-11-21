{-# LANGUAGE MultiWayIf #-}
module Control.Concurrent.Phaser.InternalSpec
  ( spec
  , main )
where

import Control.Concurrent ( forkIO
                          , modifyMVar_
                          , newMVar
                          , readMVar
                          , yield )
import Control.Monad      ( (=<<)
                          , forM_ )
       
import Data.Time.Clock
import Test.Hspec
import Test.QuickCheck

import Control.Concurrent.Phaser.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Countdown" $ do

    it "Performs registration" $ do
      cd <- newCountdown 1 (return ())
      forM_ [1..100]
        (\i -> do
          registerCountdown cd
          num_registered <- readMVar $ registered cd
          num_registered `shouldBe` (i + 1)
        )

    it "Performs unregistration" $ do
      cd <- newCountdown 101 (return ())
      forM_ [1..100]
        (\i -> do
            unregisterArriveCountdown cd
            num_registered <- readMVar $ registered cd
            num_registered `shouldBe` (101 - i)
        )

    --it "Can advance" $ do
      --cd <- newCountdown 1 (return ())
      --arriveCountdown

reattemptFor :: Double -> IO Bool -> IO Bool
reattemptFor interval action =
  let dt = picosecondsToDiffTime (round $ interval * 1e+12)
  in getCurrentTime >>= (\now ->
    let deadline = addUTCTime ((fromRational . toRational) dt) now
    in action `byDeadline` deadline
  )
  where
    action `byDeadline` deadline = do
      yield
      result <- action
      now    <- getCurrentTime
      if | result         -> return True
         | now > deadline -> return False
         | otherwise      -> action `byDeadline` deadline
