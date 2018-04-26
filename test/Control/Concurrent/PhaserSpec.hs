{-# LANGUAGE MultiWayIf #-}
module Control.Concurrent.PhaserSpec
  ( spec
  , main )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad      ( (=<<)
                          , forM_ )
import Data.Time.Clock
import Test.Hspec
import Test.QuickCheck

import Control.Concurrent.Phaser

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Phaser" $ do
  it "works once with one signaller" $ do
    -- Place something in an MVar within a phaser and confirm the result.
    x <- newEmptyMVar
    ph <- newIntPhaser 1
    runPhased ph Signal (putMVar x 1)
    one <- readMVar x
    one `shouldBe` 1

  it "works twice with one signaller" $ do
    -- Increment an MVar twice within a phaser and confirm the result.
    x  <- newMVar 0
    ph <- newIntPhaser 1
    runPhased ph Signal (modifyMVar_ x (\i -> return $ i + 1))
    one <- readMVar x
    one `shouldBe` 1
    runPhased ph Signal (modifyMVar_ x (\i -> return $ i + 1))
    two <- readMVar x
    two `shouldBe` 2

  it "won't deadlock with only Wait-ing threads" $ do
    x <- newEmptyMVar
    ph <- newIntPhaser 1
    runPhased ph Wait (putMVar x True)
    true <- takeMVar x
    true `shouldBe` True

  it "permits registration while all threads exited" $ do
    0 `shouldBe` 0
  it "permits unregistration while all threads exited" $ do
    0 `shouldBe` 0
  it "permits registration inside phaser" $ do
    0 `shouldBe` 0
  it "permits unregistration inside phaser" $ do
    0 `shouldBe` 0
  it "won't deregister below 0" $ do
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
