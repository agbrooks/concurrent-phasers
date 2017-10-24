{-# LANGUAGE MultiWayIf #-}
module Test.BasicSpec
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

import Control.Concurrent.Phaser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Phaser" $ do
    it "Performs registration" $ do
      ph <- newIntPhaser 1
      forM_ [1..100]
        (\i -> do
          register ph
          num_registered <- registered ph
          num_registered `shouldBe` (i + 1)
        )

    it "Performs deregistration" $ do
      ph <- newIntPhaser 100
      forM_ (reverse [1..99])
        (\i -> do
          unregister ph
          num_registered <- registered ph
          num_registered `shouldBe` i
        )

    it "Won't deregister parties below a zero-count" $ do
      ph <- newIntPhaser 1
      unregister ph
      num_registered <- registered ph
      num_registered `shouldBe` 0
      unregister ph
      num_registered <- registered ph
      num_registered `shouldBe` 0

    it "Won't permit creation of a phaser with less than 0 threads registered" $ do
      ph <- newIntPhaser 0
      num_registered <- registered ph
      num_registered `shouldBe` 0
      ph <- newIntPhaser (-15)
      num_registered <- registered ph
      num_registered `shouldBe` 0

    it "Provides phase when requested" $ do
      -- Int for phase
      the_phase <- phase =<< newPhaser (0 :: Int) 0
      the_phase `shouldBe` 0
      -- Integer for phase
      the_phase <- phase =<< newPhaser (11 :: Integer) 0
      the_phase `shouldBe` 11
      -- Ordering for phase (why not?)
      the_phase <- phase =<< newPhaser LT 0
      the_phase `shouldBe` LT

    it "Advances with one registered party" $ do
      ph <- newIntPhaser 1
      num_registered <- registered ph
      num_registered `shouldBe` 1

      forM_ [1..100]
        (\i -> do
          await ph
          new_phase <- phase ph
          new_phase `shouldBe` i
        )

    it "Advances with multiple registered parties" $ do
      ph <- newIntPhaser 2
      num_registered <- registered ph
      num_registered `shouldBe` 2

      forM_ [1..100]
        (\i -> do
          forkIO (await ph)
          await ph
          new_phase <- phase ph
          new_phase `shouldBe` i
        )

    it "Remembers how many registered even after advance" $ do
      ph <- newIntPhaser 2
      forkIO (await ph)
      await ph
      num_registered <- registered ph
      num_registered `shouldBe` 2

    it "Blocks and unblocks" $ do
      {-
      This test uses a combination of a "read" and "write" phaser across two
      threads to verify that the phasers are blocking / unblocking threads at
      the appropriate time.

      The "reader" thread reads the MVar x, and the "writer" thread increments
      it. The two threads "take turns" accessing x.

      While this is a nice test, don't use Phasers like this in the
      "real world"; you'd be much better off with a pair of MVars.
      -}

      x        <- newMVar 0
      ph_read  <- newIntPhaser 2
      ph_write <- newIntPhaser 2

      -- Increment the read phaser's count by one.
      forkIO (await ph_read)
      -- Writer thread
      forkIO (forM_ [0..99]
                 (\i -> do
                     await ph_write
                     modifyMVar_ x (\i -> return (i + 1 :: Int))
                     await ph_read
                 )
             )
      -- Reader/validator thread
      forM_ [0..99]
        (\i -> do
            await ph_read
            x_now <- readMVar x
            x_now `shouldBe` (i :: Int)
            await ph_write
        )

    it "Supports 'fancy' await operations" $ do
      ph <- newIntPhaser 1
      -- Wait until phase 10.
      awaitUntil 10 ph
      new_phase <- phase ph
      new_phase `shouldBe` 10
      -- Wait an additional 5 phases.
      awaitFor 5 ph
      new_phase <- phase ph
      new_phase `shouldBe` 15

    it "Accurately tracks how many parties have arrived" $ do
      ph <- newIntPhaser 4
      forM_ [1..3] (\_ -> forkIO (await ph))
      all_arrived <- reattemptFor 2.0 ((== 3) <$> arrived ph)
      all_arrived `shouldBe` True

    it "Can trigger an advance with an unregister" $ do
      ph <- newIntPhaser 4
      forM_ [1..3] (\_ -> forkIO (await ph))
      unregister ph
      phase_advanced <- reattemptFor 2.0 ((== 1) <$> phase ph)
      phase_advanced `shouldBe` True

  describe "Signal operation" $ do

    it "Increases arrival count" $ do
      ph <- newIntPhaser 2
      forkIO (signal ph)
      signalled <- reattemptFor 2.0 ((== 1) <$> arrived ph)
      signalled `shouldBe` True

    it "Can advance the phaser" $ do
      ph <- newIntPhaser 3
      forM_ [1..3] (\_ -> forkIO (signal ph))
      has_advanced <- reattemptFor 2.0 ((== 1) <$> phase ph)
      has_advanced `shouldBe` True

    it "Interoperates with await" $ do
      ph <- newIntPhaser 4
      forM_ [1..3] (\_ -> forkIO (signal ph))
      await ph
      current_phase <- phase ph
      current_phase `shouldBe` 1

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
