{-# LANGUAGE MultiWayIf #-}
module Control.Concurrent.PhaserSpec
  ( phaser_spec
  , main )
where

import Control.Concurrent ( forkIO
                          , yield  )
import Control.Monad      ( (=<<)
                          , forM_ )
import Data.Time.Clock
import Test.Hspec
import Test.QuickCheck

import Control.Concurrent.Phaser

main :: IO ()
main = hspec phaser_spec

phaser_spec :: Spec
phaser_spec = describe "Phaser" $ do
  it "works once with one signaller" $ do
    0 `shouldBe` 0
  it "works twice with one signaller" $ do
    0 `shouldBe` 0
  it "permits basic registration" $ do
    0 `shouldBe` 0
  it "permits basic unregistration" $ do
    0 `shouldBe` 0
  it "won't deregister below 0" $ do
    0 `shouldBe` 0
  it "won't deadlock with 0 signallers" $ do
    0 `shouldBe` 0
  it "provides phase" $ do
    0 `shouldBe` 0
  it "blocks Wait-mode threads" $ do
    0 `shouldBe` 0
  it "blocks SigWait-mode threads" $ do
    0 `shouldBe` 0
  it "won't block Signal-mode threads" $ do
    0 `shouldBe` 0
  it "survives multi-phase operations" $ do
    0 `shouldBe` 0
  it "survives mixed-mode multi-phase operations" $ do
    0 `shouldBe` 0

-- | Keep attempting some IO action until it returns True or the interval
--   elapses. The interval is provided in seconds.
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
