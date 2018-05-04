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
import Control.Concurrent.Phaser.STM

main :: IO ()
main = hspec spec

spec :: Spec
spec =  makeSpecForPhaser (newSTMPhaser 0) "STM Phaser"
     >> makeSpecForPhaser (newIOPhaser  0) "IO Phaser"

makeSpecForPhaser implNewIntPhaser name = describe name $ do

  it "works once with one Signal-mode thread" $ do
    -- Place something in an MVar within a phaser and confirm the result.
    x <- newEmptyMVar
    ph <- implNewIntPhaser 1
    runPhased ph Signal (putMVar x 1)
    one <- readMVar x
    one `shouldBe` 1

  it "works twice with one Signal-mode thread" $ do
    -- Increment an MVar twice within a phaser and confirm the result.
    x  <- newMVar 0
    ph <- implNewIntPhaser 1
    runPhased ph Signal (modifyMVar_ x (\i -> return $ i + 1))
    one <- readMVar x
    one `shouldBe` 1
    runPhased ph Signal (modifyMVar_ x (\i -> return $ i + 1))
    two <- readMVar x
    two `shouldBe` 2

  it "won't deadlock with only Wait-ing threads" $ do
    x <- newEmptyMVar
    ph <- implNewIntPhaser 1
    runPhased ph Wait (putMVar x True)
    true <- takeMVar x
    true `shouldBe` True

  it "works twice with one Wait-mode thread" $ do
    -- Increment an MVar twice within a phaser and confirm the result
    x <- newMVar 0
    ph <- implNewIntPhaser 1
    runPhased ph Wait (modifyMVar_ x (\i -> return $ i + 1))
    one <- readMVar x
    one `shouldBe` 1
    runPhased ph Wait (modifyMVar_ x (\i -> return $ i + 1))
    two <- readMVar x
    two `shouldBe` 2

  it "won't deadlock with a SignalWait-mode thread" $ do
    ph <- implNewIntPhaser 1
    runPhased ph SignalWait (return ())

  it "works twice with one SignalWait-mode thread" $ do
    -- Increment MVar twice and confirm the result
    x <- newMVar 0
    ph <- implNewIntPhaser 1
    runPhased ph SignalWait (modifyMVar_ x (\i -> return $ i + 1))
    one <- readMVar x
    one `shouldBe` 1
    runPhased ph SignalWait (modifyMVar_ x (\i -> return $ i + 1))
    two <- readMVar x
    two `shouldBe` 2

  it "provides phase" $ do
    ph <- implNewIntPhaser 1
    orig_phase <- phase ph
    orig_phase `shouldBe` 0
    runPhased ph Signal (return ())
    new_phase <- phase ph
    new_phase `shouldBe` 1
    new_phase <- phase ph -- Should be able to read multiply
    new_phase `shouldBe` 1

  it "increments phase properly" $ do
    ph <- implNewIntPhaser 1
    runPhased ph Signal (return ())
    new_phase <- phase ph
    new_phase `shouldBe` 1
    runPhased ph Signal (return ())
    new_phase <- phase ph
    new_phase `shouldBe` 2

  it "permits registration while all threads exited" $ do
    -- Create a phaser with one party, then register another.
    ph <- implNewIntPhaser 1
    register ph
    -- Phaser should now expect two parties.
    forkIO (runPhased ph SignalWait (return ()))
    runPhased ph SignalWait (return ())
    -- To rule out the 'both went through in separate phases' case, check the
    -- phase.
    success <- reattemptFor 0.2 ((==) <$> phase ph <*> (return 1))
    success `shouldBe` True

  it "permits unregistration while all threads exited" $ do
    ph <- implNewIntPhaser 2
    unregister ph
    -- Phaser should now expect one party. So, two threads passing through
    -- should advance the phase twice.
    forkIO (runPhased ph SignalWait (return ()))
    runPhased ph SignalWait (return ())
    success <- reattemptFor 0.2 ((==) <$> phase ph <*> return 2)
    success `shouldBe` True

  it "permits registration inside phaser" $ do
    ph <- implNewIntPhaser 1
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
    success <- reattemptFor 0.2 ((==) <$> phase ph <*> return 2)
    success `shouldBe` True

  it "permits unregistration inside phaser" $ do
    -- Very similar to the "registration inside phaser" test.
    ph <- implNewIntPhaser 2
    p <- phase ph
    p `shouldBe` 0

    forkIO (runPhased ph SignalWait (unregister ph))
    runPhased ph SignalWait (return ())
    success <- reattemptFor 0.2 ((==) <$> phase ph <*> return 1)
    success `shouldBe` True

    -- Only one needed for this phase. If unregister failed, we just deadlocked.
    runPhased ph SignalWait (return ())
    p <- phase ph
    p `shouldBe` 2

  it "gracefully handles having no registered parties" $ do
    ph <- implNewIntPhaser 0
    mv <- newMVar 0
    runPhased ph SignalWait (modifyMVar_ mv (\i -> return $ i + 1))
    v  <- readMVar mv
    v `shouldBe` 1

  it "won't deregister below 0" $ do
    ph <- implNewIntPhaser 1
    unregister ph
    unregister ph
    unregister ph
    register ph
    ph <- implNewIntPhaser 0
    runPhased ph SignalWait (return ())
    p <- phase ph
    p `shouldBe` 1

  it "blocks Wait-mode threads" $ do
    ph <- implNewIntPhaser 2
    -- Start a background thread that should not increment the phase.
    forkIO (runPhased ph Wait $ return ())
    yield
    -- Confirm phase not updated (ie, wait-mode thread was blocked).
    phase_updated <- reattemptFor 0.05 ((==) <$> phase ph <*> return 1)
    phase_updated `shouldBe` False
    -- Send in signalling thread to unblock.
    runPhased ph Signal $ return ()
    phase_updated <- reattemptFor 0.1 ((==) <$> phase ph <*> return 1)
    phase_updated `shouldBe` True

  it "blocks SigWait-mode threads" $ do
    ph <- implNewIntPhaser 2
    -- Start a background thread that should not increment the phase.
    forkIO (runPhased ph SignalWait $ return ())
    yield
    -- Confirm phase not updated (ie, wait-mode thread was blocked).
    phase_updated <- reattemptFor 0.05 ((==) <$> phase ph <*> return 1)
    phase_updated `shouldBe` False
    -- Send in signalling thread to unblock.
    runPhased ph Signal $ return ()
    phase_updated <- reattemptFor 0.1 ((==) <$> phase ph <*> return 1)
    phase_updated `shouldBe` True

  it "can perform mixed-mode, multi-phaser operations" $ do
    {-
       Make two phasers. Run on both simultaneously in each of two threads.
       One thread provides the signal, and the other the wait.
       If we don't order the signal and wait correctly in `runMultiPhased`, this
       would deadlock.
    -}
    ph1 <- implNewIntPhaser 2
    ph2 <- implNewIntPhaser 2
    forkIO (runMultiPhased [(ph1, Signal), (ph2, Wait)  ] $ return ())
    runMultiPhased         [(ph1, Wait),   (ph2, Signal)] $ return ()
    phase1_updated <- reattemptFor 0.1 ((==) <$> phase ph1 <*> return 1)
    phase2_updated <- reattemptFor 0.1 ((==) <$> phase ph2 <*> return 1)

    phase1_updated `shouldBe` True
    phase2_updated `shouldBe` True

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
