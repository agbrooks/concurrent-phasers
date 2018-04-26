{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
module Control.Concurrent.Phaser
  ( Phaser ()
  , PhaserMode ( Signal, Wait, SignalWait )
  , newPhaser
  , newIntPhaser
  , phase
  , register
  , unregister
  , batchRegister
  , batchUnregister
  , runPhased
  , runMultiPhased )
where

import Control.Concurrent.STM
import Control.Exception       ( uninterruptibleMask_ )
import Control.Monad           ( when )

{- TODO:
* Check Haddock syntax.
* Apply uninterruptableMask_ to prevent deadlock.
-}

{- |Mode in which a task uses a @Phaser@.
    A task in `Wait` mode on a @Phaser@ will be blocked when trying to exit the
    @Phaser@ until all tasks in `Signal` mode have tried to leave.
    `SignalWait` combines `Signal` and `Wait` modes.
-}
data PhaserMode
  = Signal
  | Wait
  | SignalWait

data Phaser p = Phaser
  { _phase       :: TVar p   -- ^ Phase of the phaser.
  , _registered  :: TVar Int -- ^ Parties registered on the phaser.
  , _adjustment  :: TVar Int -- ^ Number of parties planning to enter or leave
                             --   during the next @Phaser@ pass.
  , _sig_rx   :: TMVar Int   -- ^ Signals received.
  , _sig_reg  :: TVar Int    -- ^ Signals registered (signals required to advance)
  , _wait_fin :: TMVar Int -- ^ Waits finished.
  , _wait_reg :: TVar Int  -- ^ Waits registered.
  , _entered  :: TMVar Int -- ^ Parties that have entered the phaser.
  }

-- | Create a new @Phaser@.
newPhaser
  :: Enum p
  => p   -- ^ Starting phase for the @Phaser@.
  -> Int -- ^ Number of parties to expect for first round.
  -> IO (Phaser p)
newPhaser p parties =
  atomically $
  Phaser <$> newTVar  p       -- Initial phase
         <*> newTVar  parties -- Initial parties
         <*> newTVar  0
         <*> newEmptyTMVar  -- Signals received, empty until all register.
         <*> newTVar  0
         <*> newEmptyTMVar  -- Waits received, empty until all register.
         <*> newTVar  0
         <*> newTMVar 0 -- Number entered. Zero for now.

-- | Create a new phaser with initial @Phase@ 0.
newIntPhaser :: Int -> IO (Phaser Int)
newIntPhaser parties = newPhaser 0 parties

-- | Retrieve a @Phaser@'s current phase.
phase :: Phaser p -> IO p
phase = atomically . readTVar . _phase

-- | Tell a @Phaser@ to expect one more party when its next phase begins.
register :: Phaser p -> IO ()
register p = batchRegister p 1

-- | Tell a @Phaser@ to expect one fewer party when its next phase begins.
unregister :: Phaser p -> IO ()
unregister p = batchUnregister p (-1)

-- | Register several parties on a @Phaser@.
batchRegister :: Phaser p -> Int -> IO ()
batchRegister p i = atomically $ modifyTVar (_adjustment p) (+i)

-- | Unregister several parties on a @Phaser@.
batchUnregister :: Phaser p -> Int -> IO ()
batchUnregister p i = batchRegister p (-i)

{- |Synchronize a task with a @Phaser@.
    Caller is responsible for graceful unregistration in the event of an
    asynchronous exception.
-}
runPhased
  :: Enum p
  => Phaser p   -- ^@Phaser@ in question
  -> PhaserMode -- ^Mode the task will use when synchronizing on the @Phaser@
  -> IO a       -- ^Task performed
  -> IO a
runPhased p m action = do
  p `enterInMode` m
  result <- action
  when (modeSignals m) $ signal p
  when (modeWaits   m) $ wait   p
  return result

{- |Synchronize a task across multiple @Phaser@s in multiple @PhaserMode@s.
    The @Phaser@/@PhaserMode@ pairs may be specified in any order.
-}
-- | Synchronize a task across multiple @Phaser@s in multiple @PhaserMode@s.
runMultiPhased :: Enum p => [(Phaser p, PhaserMode)] -> IO a -> IO a
runMultiPhased [] action = action
runMultiPhased pairs action =
  let signal_pairs = filter (\(_, m) -> modeSignals m) pairs
      wait_pairs   = filter (\(_, m) -> modeWaits   m) pairs
  in do
    mapM_ (uncurry enterInMode) signal_pairs
    mapM_ (uncurry enterInMode) wait_pairs
    result <- action
    mapM_ (signal . fst) signal_pairs
    mapM_ (wait   . fst) wait_pairs
    return result

{- Internal functions follow -}

-- Does a given mode indicate that we should signal?
modeSignals :: PhaserMode -> Bool
modeSignals Wait = False
modeSignals _    = True

-- Does a given mode indicate that we should wait?
modeWaits :: PhaserMode -> Bool
modeWaits Signal = False
modeWaits _      = True

-- Alter the number of registered parties by adding the adjustment.
applyAdjustment :: Phaser p -> STM ()
applyAdjustment p = do
  adj <- swapTVar (_adjustment p) 0
  modifyTVar (_registered p) (+ adj)

-- Enter the phaser in a given mode.
enterInMode :: Phaser p -> PhaserMode -> IO ()
enterInMode p m = atomically $ do
  -- Adjust for new parties registered or unregistered
  entered    <- takeTMVar (_entered p)
  when (entered == 0) $ applyAdjustment p
  registered <- readTVar (_registered p)

  -- Adjust signal / wait registration
  when (modeSignals m) $ modifyTVar (_sig_reg  p) (+1)
  when (modeWaits   m) $ modifyTVar (_wait_reg p) (+1)

  -- If all expected parties arrived, unblock their exit.
  if entered + 1 >= registered then finishEntry p
  else putTMVar (_entered p) (entered + 1)

-- Unblock exit of signalling parties. These, in turn, free the waiting parties.
finishEntry :: Phaser p -> STM ()
finishEntry p = putTMVar (_sig_rx p) 0

wait :: Enum p => Phaser p -> IO ()
wait p = atomically $ do
  w_reg <- readTVar  (_wait_reg p)
  w_fin <- takeTMVar (_wait_fin p)
  if | w_reg <= (w_fin + 1) -> advance p
     | otherwise -> putTMVar (_wait_fin p) (w_fin + 1)

signal :: Enum p => Phaser p -> IO ()
signal p = atomically $ do
  w_reg <- readTVar  (_wait_reg p)
  s_reg <- readTVar  (_sig_reg  p)
  s_rx  <- takeTMVar (_sig_rx   p)
  let is_last_signal = s_reg <= (s_rx + 1)
      no_waits       = w_reg == 0
      unblockWaits   = putTMVar (_wait_fin p) 0
  if | is_last_signal && no_waits -> advance p
     | is_last_signal             -> unblockWaits
     | otherwise                  -> putTMVar (_sig_rx p) (s_rx + 1)

-- Increase the phase to its successor.
nextPhase :: Enum p => Phaser p -> STM ()
nextPhase p = modifyTVar (_phase p) succ

-- Move on to the next phase and permit entry into the phaser.
advance :: Enum p => Phaser p -> STM ()
advance p = nextPhase p >> putTMVar (_entered p) 0
