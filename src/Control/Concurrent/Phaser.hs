{-# LANGUAGE BangPatterns #-}

module Phaser
  ( newPhaser
  , newIntPhaser
  , arrived
  , phase
  , registered
  , register
  , unregister
  , await
  , Phaser () )
where

import Control.Concurrent.MVar
import Control.Monad           ( when )
import Data.IORef

{-
FIXME: This needs to be made exception-safe!
TODO: Add support for tree-structured phasers.
TODO: Elaborate in documentation.
TODO: Don't import everything from the imported modules.
-}

{-|
A "Phaser" acts as a reusable barrier with an adjustable number of threads
that wait on it.
-}
data Phaser p = Phaser
  { _phase      :: MVar p
  , _registered :: MVar Int
  , _arrived    :: MVar Int
  , _waitOn     :: MVar ()  -- ^ Synchronization object for waiting threads.
  }

-- | Determine how many parties have arrived at the @Phaser@.
arrived :: Phaser p -> IO Int
arrived = (readMVar . _arrived)

-- | Read the "phase" of the Phaser. Once all threads advance, the phase
--   increases.
phase :: Phaser p -> IO p
phase = (readMVar . _phase)

-- | Determine how many parties / threads are currently registered with the
--   @Phaser@.
registered :: Phaser p -> IO Int
registered = (readMVar . _registered)

-- | Create a new @Phaser@.
newPhaser :: Enum p
          => p -- ^ @phase@ to start in.
          -> Int -- ^ Number of parties to initially register with the Phaser.
          -> IO (Phaser p)
newPhaser p i = Phaser
  <$> (newMVar p)
  <*> (newMVar i)
  <*> (newMVar 0)
  <*> (newEmptyMVar)

-- | Create a new @Phaser@ which uses an Int to track @phase@, starting at 0.
newIntPhaser :: Int -> IO (Phaser Int)
newIntPhaser = newPhaser 0

-- | Register a thread with a @Phaser@.
register :: Phaser p -> IO ()
register r = modifyMVar_ (_registered r) (return . (+1))

-- | Unregister a thread from a @Phaser@. Do not permit the number of registered
--   threads to drop below zero. If the number of registered parties would become
--   the number of parties arrived, then advance to the next phase.
unregister :: Enum p => Phaser p -> IO ()
unregister ph = do
  !n_registered <- registered ph
  !n_arrived    <- arrived ph
  modifyMVar_ (_registered ph) (\i -> return $ max 0 (i - 1))
  when (n_arrived >= n_registered - 1) $
    advance ph

-- | Wait at the phaser until all threads arrive. Once all threads have arrived,
--   the @Phaser@ will advance.
await :: Enum p => Phaser p -> IO ()
await ph = undefined -- TODO: Left off here

-- | Reset the arrival count of a @Phaser@, increase its @phase@, and awaken
--   all threads sleeping on the @Phaser@. For internal use only.
advance :: Enum p => Phaser p -> IO ()
advance ph =  swapMVar    (_arrived ph) 0
           >> modifyMVar_ (_phase ph)   (return . succ)
           >> putMVar     (_waitOn ph)  ()

-- | Wait on a @Phaser@ until it becomes available. For internal use only.
sleepOn :: Phaser p -> IO ()
sleepOn ph = let cond = _waitOn ph in
  readMVar    cond >>
  tryTakeMVar cond >>
  return ()
{-
If a putMVar occurs after a readMVar, all threads blocked on readMVar are
guaranteed to run before the putMVar ever occurs. For this reason, we can be
certain that all sleeping threads will awaken.
-}
