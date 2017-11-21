{-# LANGUAGE MultiWayIf #-}
module PhaserInternalsSpec
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

import Test.Hspec
import Test.QuickCheck

import Control.Concurrent.Phaser.Internal

--main :: IO ()
--main = hspec spec

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
