{-# LANGUAGE MultiWayIf #-}
module Control.Concurrent.Phaser.InternalSpec
  ( spec
  , main )
where

import Control.Concurrent 
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
      cd <- newCountdown 1 (\_ -> return ())
      forM_ [1..100]
        (\i -> do
          registerCountdown cd
          num_registered <- readMVar $ registered cd
          num_registered `shouldBe` (i + 1)
        )

    it "Performs unregistration" $ do
      cd <- newCountdown 101 (\_ -> return ())
      forM_ [1..100]
        (\i -> do
            unregisterArriveCountdown cd
            num_registered <- readMVar $ registered cd
            num_registered `shouldBe` (101 - i)
        )

    it "Advances successfully with one party" $ do
      done <- newEmptyMVar
      cd   <- newCountdown 1 (putMVar done)
      arriveCountdown cd
      finished <- readMVar done
      finished `shouldBe` 1

    it "Won't deregister below a zero-count" $ do
      cd <- newCountdown 0 (\_ -> return ())
      unregisterArriveCountdown cd
      registered <- getRegistered cd
      registered `shouldBe` 0

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
