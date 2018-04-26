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

  it "provides phase" $ do
    ph <- newIntPhaser 1
    orig_phase <- phase ph
    orig_phase `shouldBe` 0
    runPhased ph Signal (return ())
    new_phase <- phase ph
    new_phase `shouldBe` 1
    new_phase <- phase ph -- Should be able to read multiply
    new_phase `shouldBe` 1

  it "permits registration while all threads exited" $ do
    -- Create a phaser with one party, then register another.
    ph <- newIntPhaser 1
    register ph
    -- Phaser should now expect two parties.
    forkIO (runPhased ph SignalWait (return ()))
    runPhased ph SignalWait (return ())
    -- To rule out the 'both went through in separate phases' case, check the
    -- phase.
    p <- phase ph
    p `shouldBe` 1

  it "permits unregistration while all threads exited" $ do
    ph <- newIntPhaser 2
    unregister ph
    -- Phaser should now expect one party.
    forkIO (runPhased ph SignalWait (return ()))
    runPhased ph SignalWait (return ())
    p <- phase ph
    p `shouldBe` 2

  it "permits registration inside phaser" $ do
    ph <- newIntPhaser 1
    p <- phase ph
    p `shouldBe` 0

    -- Run once, register inside. Phaser should still complete, since the
    -- registration doesn't 'do anything' until the next phase.
    runPhased ph SignalWait (register ph)
    p <- phase ph
    p `shouldBe` 1

    -- Now, it expects 2 parties to declare a mode. Check the phase to make
    -- sure that it didn't just run twice.
    forkIO (runPhased ph Wait (return ())) --
    runPhased ph SignalWait (return ())
    p <- phase ph
    p `shouldBe` 2

  it "permits unregistration inside phaser" $ do
    -- Very similar to the "registration inside phaser" test.
    ph <- newIntPhaser 2
    p <- phase ph
    p `shouldBe` 0

    forkIO (runPhased ph SignalWait (unregister ph))
    runPhased ph SignalWait (return ())
    p <- phase ph
    p `shouldBe` 1

    -- Only one needed for this phase. If unregister failed, we just deadlocked.
    runPhased ph SignalWait (return ())
    p <- phase ph
    p `shouldBe` 2

  it "gracefully handles having no registered parties" $ do
    ph <- newIntPhaser 0
    mv <- newMVar 0
    runPhased ph SignalWait (modifyMVar_ mv (\i -> return $ i + i))
    v  <- readMVar mv
    v `shouldBe` 1

  it "won't deregister below 0" $ do
    ph <- newIntPhaser 1
    unregister ph
    unregister ph
    unregister ph
    register ph
    ph <- newIntPhaser 0
    runPhased ph SignalWait (return ())
    p <- phase ph
    p `shouldBe` 1

  it "blocks Wait-mode threads" $ do
    0 `shouldBe` 0 -- TODO

  it "blocks SigWait-mode threads" $ do
    0 `shouldBe` 0 -- TODO

  it "won't block Signal-mode threads" $ do
    0 `shouldBe` 0 -- TODO

  it "survives multi-phase operations" $ do
    0 `shouldBe` 0 -- TODO

  it "survives mixed-mode multi-phase operations" $ do
    0 `shouldBe` 0 -- TODO

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
