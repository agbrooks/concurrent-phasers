{-# LANGUAGE MultiWayIf #-}
{-| Default, `MVar`-based `Phaser` implementation (`IOPhaser`).
-}
module Control.Concurrent.Phaser
  ( Phaser   ()
  , IOPhaser ()
  , PhaserMode ( Signal, Wait, SignalWait )
  , newPhaser
  , newIntPhaser
  , newIOPhaser
  , phase
  , register
  , unregister
  , batchRegister
  , batchUnregister
  , runPhased
  , runMultiPhased )
where

import Control.Concurrent.Phaser.Generic

import Control.Concurrent.MVar
import Control.Monad           ( when )
import Data.IORef

-- | MVar / IORef-based 'Phaser' implementation.
data IOPhaser p = IOPhaser
  { _phase       :: IORef p   -- Phase of the phaser.
  , _registered  :: IORef Int -- Parties registered on the phaser.
  , _registered' :: IORef Int -- Number of parties registered next time.
  , _sig_rx      :: MVar Int  -- Signals received.
  , _sig_reg     :: IORef Int -- Signals registered (signals required to advance)
  , _wait_fin    :: MVar Int  -- Waits finished
  , _wait_reg    :: IORef Int -- Waits registered
  , _entered     :: MVar Int  -- Parties that have entered the phaser.
  }

instance Phaser IOPhaser where
  newPhaser     = newPhaser_
  enterInMode   = enterInMode_
  wait          = wait_
  signal        = signal_
  batchRegister = batchRegister_
  phase         = phase_

-- | Create a new 'IOPhaser'.
newPhaser_
  :: Enum p
  => p   -- ^ Starting phase for the 'Phaser'.
  -> Int -- ^ Number of parties to expect for first round.
  -> IO (IOPhaser p)
newPhaser_ p parties =
  IOPhaser <$> newIORef  p               -- Initial phase
           <*> newIORef  (max 0 parties) -- Initial parties
           <*> newIORef  (max 0 parties)
           <*> newEmptyMVar  -- Signals received, empty until all register.
           <*> newIORef  0
           <*> newEmptyMVar  -- Waits received, empty until all register.
           <*> newIORef  0
           <*> newMVar 0 -- Number entered. Zero initially.

-- | Specialized constructor for a @IOPhaser@.
newIOPhaser :: Enum p => p -> Int -> IO (IOPhaser p)
newIOPhaser = newPhaser_

-- | Retrieve a @IOPhaser@'s current phase.
phase_ :: IOPhaser p -> IO p
phase_ = readIORef . _phase

-- | Register several parties on a @IOPhaser@.
batchRegister_ :: IOPhaser p -> Int -> IO ()
batchRegister_ p i =
  atomicModifyIORef' (_registered' p)
    (\reg -> (max 0 (i + reg), ()))

-- Update the number of registered parties to use the number planned.
updateRegistered :: IOPhaser p -> IO ()
updateRegistered p =
  atomicWriteIORef (_registered p) =<< (readIORef $ _registered' p)

-- Reset the signal / wait registrations for this phase
resetSigWaitRegistered :: IOPhaser p -> IO ()
resetSigWaitRegistered p =
  writeIORef (_sig_reg  p) 0 >>
  writeIORef (_wait_reg p) 0

-- Enter the phaser in a given mode.
-- TODO: work on async exception safety
enterInMode_ :: IOPhaser p -> PhaserMode -> IO ()
enterInMode_ p m = do
  -- Adjust for new parties registered or unregistered
  entered    <- takeMVar (_entered p)
  when (entered == 0) $  updateRegistered p --update registration total once new phase starts
                      >> resetSigWaitRegistered p
  registered <- readIORef (_registered p)

  -- Adjust signal / wait registration. These modifyIORefs are effectively
  -- atomic, since they are locked on _entered p.
  when (modeSignals m) $ modifyIORef' (_sig_reg  p) (+ 1)
  when (modeWaits   m) $ modifyIORef' (_wait_reg p) (+ 1)

  -- If all expected parties arrived, unblock their exit.
  if entered + 1 >= registered then finishEntry p
  else putMVar (_entered p) (entered + 1)

-- Unblock exit of signalling parties. These, in turn, free the waiting parties.
finishEntry :: IOPhaser p -> IO ()
finishEntry p = do
  sig_reg <- readIORef $ _sig_reg p
  -- If no signallers are registered, we could deadlock.
  -- Act as if all signallers have arrived if this is the case.
  if | sig_reg == 0 -> putMVar (_wait_fin p) 0
     | otherwise    -> putMVar (_sig_rx   p) 0

-- TODO: work on async exception safety
wait_ :: Enum p => IOPhaser p -> IO ()
wait_ p = do
  w_fin <- takeMVar  (_wait_fin p)
  w_reg <- readIORef (_wait_reg p)
  if | w_reg <= (w_fin + 1) -> advance p
     | otherwise -> putMVar (_wait_fin p) (w_fin + 1)

-- TODO: work on async exception safety
signal_ :: Enum p => IOPhaser p -> IO ()
signal_ p = do
  s_rx  <- takeMVar   (_sig_rx   p)
  w_reg <- readIORef  (_wait_reg p)
  s_reg <- readIORef  (_sig_reg  p)
  let is_last_signal  = s_reg <= (s_rx + 1)
      no_waits        = w_reg == 0
      unblockWaits () = putMVar (_wait_fin p) 0
  if | is_last_signal && no_waits -> advance p
     | is_last_signal             -> unblockWaits ()
     | otherwise -> putMVar (_sig_rx p) (s_rx + 1)

-- Increase the phase to its successor.
nextPhase :: Enum p => IOPhaser p -> IO ()
nextPhase p = atomicModifyIORef' (_phase p) (\h -> (succ h, ()))

-- Move on to the next phase and permit entry into the phaser.
advance :: Enum p => IOPhaser p -> IO ()
advance p
  =  nextPhase p
  >> putMVar (_entered p) 0
