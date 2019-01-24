{-# LANGUAGE MultiWayIf   #-}
{- |STM-based `Phaser` implementation offered as an alternative to
    the MVar-based `'Control.Concurrent.Phaser.IOPhaser`.
-}
module Control.Concurrent.Phaser.STM
  ( Phaser    ()
  , STMPhaser ()
  , PhaserMode ( Signal, Wait, SignalWait )
  , newPhaser
  , newIntPhaser
  , newSTMPhaser
  , phase
  , register
  , unregister
  , batchRegister
  , batchUnregister
  , runPhased
  , runMultiPhased )
where

import Control.Concurrent.Phaser.Generic

import Control.Concurrent.STM
import Control.Monad           ( when )

-- | STM-based `Phaser`.
data STMPhaser p = STMPhaser
  { _phase       :: TVar p   -- Phase of the phaser.
  , _registered  :: TVar Int -- Parties registered on the phaser.
  , _registered' :: TVar Int -- Number of parties that will be registered
                             --   on the phaser next time.
  , _sig_rx   :: TMVar Int  -- Signals received.
  , _sig_reg  :: TVar Int  -- Signals registered (signals required to advance)
  , _wait_fin :: TMVar Int -- Waits finished.
  , _wait_reg :: TVar Int  -- Waits registered.
  , _entered  :: TMVar Int -- Parties that have entered the phaser.
  }

instance Phaser STMPhaser where
  newPhaser     = newPhaser_
  enterInMode   = enterInMode_
  wait          = wait_
  signal        = signal_
  batchRegister = batchRegister_
  phase         = phase_

-- Create a new `STMPhaser`.
newPhaser_
  :: Enum p
  => p   -- ^ Starting phase for the `Phaser`.
  -> Int -- ^ Number of parties to expect for first round.
  -> IO (STMPhaser p)
newPhaser_ p parties =
  atomically $
  STMPhaser <$> newTVar  p               -- Initial phase
            <*> newTVar  (max 0 parties) -- Initial parties
            <*> newTVar  (max 0 parties)
            <*> newEmptyTMVar  -- Signals received.
            <*> newTVar  0
            <*> newEmptyTMVar  -- Waits received, empty until all register.
            <*> newTVar  0
            <*> newTMVar 0 -- Number entered. Zero for now.

-- | Specialized constructor for a `STMPhaser`.
newSTMPhaser :: Enum p => p -> Int -> IO (STMPhaser p)
newSTMPhaser = newPhaser_

-- | Retrieve a `STMPhaser`'s current phase.
phase_ :: STMPhaser p -> IO p
phase_ = atomically . readTVar . _phase

-- | Register several parties on a `STMPhaser`.
batchRegister_ :: STMPhaser p -> Int -> IO ()
batchRegister_ p i =
  atomically $ modifyTVar (_registered' p)
    (\reg -> max 0 (i + reg))

-- Update the number of registered parties to use the number planned.
updateRegistered :: STMPhaser p -> STM ()
updateRegistered p =
  writeTVar (_registered p) =<< (readTVar $ _registered' p)

-- Reset the signal / wait registrations for this phase
resetSigWaitRegistered :: STMPhaser p -> STM ()
resetSigWaitRegistered p =
  writeTVar (_sig_reg  p) 0 >>
  writeTVar (_wait_reg p) 0

-- Enter the phaser in a given mode.
enterInMode_ :: STMPhaser p -> PhaserMode -> IO ()
enterInMode_ p m = atomically $ do
  -- Adjust for new parties registered or unregistered
  entered    <- takeTMVar (_entered p)
  when (entered == 0) $  updateRegistered p
                      >> resetSigWaitRegistered p
  registered <- readTVar (_registered p)

  -- Adjust signal / wait registration
  when (modeSignals m) $ modifyTVar (_sig_reg  p) (+1)
  when (modeWaits   m) $ modifyTVar (_wait_reg p) (+1)

  -- If all expected parties arrived, unblock their exit.
  if entered + 1 >= registered then finishEntry p
  else putTMVar (_entered p) (entered + 1)

-- Unblock exit of signalling parties. These, in turn, free the waiting parties.
finishEntry :: STMPhaser p -> STM ()
finishEntry p = do
  sig_reg <- readTVar  $ _sig_reg p
  -- If all signallers have already arrived, or none were ever registered,
  -- unblock anyone that's Wait-ing.
  if | sig_reg == 0 -> putTMVar (_wait_fin p) 0
     | otherwise    -> putTMVar (_sig_rx   p) 0

wait_ :: Enum p => STMPhaser p -> IO ()
wait_ p = atomically $ do
  w_reg <- readTVar  (_wait_reg p)
  w_fin <- takeTMVar (_wait_fin p)
  if | w_reg <= (w_fin + 1) -> advance p
     | otherwise -> putTMVar (_wait_fin p) (w_fin + 1)

signal_ :: Enum p => STMPhaser p -> IO ()
signal_ p = atomically $ do
  s_rx  <- takeTMVar (_sig_rx   p)
  w_reg <- readTVar  (_wait_reg p)
  s_reg <- readTVar  (_sig_reg  p)
  let is_last_signal = s_reg <= (s_rx + 1)
      no_waits       = w_reg == 0
      unblockWaits   = putTMVar (_wait_fin p) 0
  -- Handle receipt of all signals
  if | is_last_signal && no_waits -> advance p       -- All arrived, advance phase
     | is_last_signal             -> unblockWaits    -- All arrived, awaken awaits
     | otherwise -> putTMVar (_sig_rx p) (s_rx + 1) -- Just receive signal

-- Increase the phase to its successor.
nextPhase :: Enum p => STMPhaser p -> STM ()
nextPhase p = modifyTVar (_phase p) succ

-- Move on to the next phase and permit entry into the phaser.
advance :: Enum p => STMPhaser p -> STM ()
advance p
  =  nextPhase p
  >> putTMVar (_entered p) 0
