{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
module Control.Concurrent.Phaser
  ( newPhaser
  , newIntPhaser
  , arrived
  , phase
  , registered
  , register
  , unregister
  , await
  , awaitFor
  , awaitUntil
  , Phaser () )
where

import Control.Concurrent.MVar
import Control.Monad           ( join
                               , liftM
                               , when )
import Data.IORef

{-
FIXME: Guarantee exception-safety.
TODO: Add support for tree-structured phasers to reduce lock contention.
TODO: Add more documentation.
TODO: Don't import everything from imported modules.
-}

{-|
A "Phaser" acts as a reusable barrier with an adjustable number of threads
that are synchronized by it.
-}
data Phaser p = Phaser
  { _phase :: MVar p -- ^ Phase we're currently on
  , _registered :: MVar Int -- ^ Number of threads registered.
  , _arrived :: MVar Int -- ^ Number of threads arrived.
  , _awoken :: MVar Int -- ^ Number of threads awoken.
  , _awoken_needed :: MVar Int -- ^ Number of threads that need to be awoken.
  }

-- | Read the "phase" of the Phaser. Once all threads advance, the phase
--   increases.
phase :: Phaser p -> IO p
phase = (readMVar . _phase)

-- | Determine how many parties / threads are currently registered with the
--   @Phaser@.
registered :: Phaser p -> IO Int
registered ph = readMVar (_registered ph)

-- | Determine how many parties have arrived at the @Phaser@.
arrived :: Phaser p -> IO Int
arrived ph = readMVar (_arrived ph)

-- | Create a new @Phaser@.
newPhaser :: Enum p
          => p -- ^ @phase@ to start in.
          -> Int -- ^ Number of parties to initially register with the @Phaser@.
                 --   Nota bene: a @Phaser@ may have no fewer than 1 party
                 --   registered.
          -> IO (Phaser p)
newPhaser p i = Phaser
  <$> (newMVar p)
  <*> (newMVar (max 1 i))
  <*> (newMVar 0)
  <*> (newEmptyMVar)
  <*> (newEmptyMVar)

-- | Create a new @Phaser@ which uses an Int to track @phase@, starting at 0.
newIntPhaser
  :: Int -- ^ Number of parties to initially register with the Phaser.
  -> IO (Phaser Int)
newIntPhaser = newPhaser 0

-- | Register a thread with a @Phaser@.
register :: Enum p => Phaser p -> IO ()
register ph = modifyMVar_ (_registered ph) (return . (+1))

-- | Unregister a thread from a @Phaser@. Do not permit the number of registered
--   threads to drop below zero. If the thread unregistering would be the last to
--   arrive, then advance all threads waiting on the @Phaser@.
unregister :: Enum p => Phaser p -> IO ()
unregister ph =
  takingMVar (_registered ph) (\n_registered ->
    takingMVar (_arrived ph)  (\n_arrived -> do
      let new_registered = max 1 (n_registered - 1)
      if (n_arrived >= new_registered) then
        advance new_registered ph
      else do
        putMVar (_arrived ph)    n_arrived
        putMVar (_registered ph) new_registered
    )
  )

-- | Advance a @Phaser@ to the next phase, once all threads have arrived,
-- | and initialize the "awoken" counter to start waking up blocked threads.
-- | For internal use only!
advance :: Enum p => Int -> Phaser p -> IO ()
advance n_registered ph
  | n_registered <= 1 = nextPhase ph >> reset
  | otherwise = do
      putMVar (_awoken_needed ph) (n_registered)
      putMVar (_awoken ph) (1)
      nextPhase ph
  where
    reset = putMVar (_arrived ph) 0

-- | Wait at the phaser until all threads arrive. Once all threads have arrived,
-- | the @Phaser@ proceeds to the next phase, and all threads are unblocked.
await :: Enum p => Phaser p -> IO ()
await ph =
  takingMVar (_registered ph) (\n_registered ->
    takingMVar (_arrived ph) (\n_arrived -> do
      let is_last_arriving = n_arrived >= n_registered - 1
      if is_last_arriving then do
        advance n_registered ph
      else do
        putMVar (_arrived ph)    (n_arrived + 1)
        putMVar (_registered ph) (n_registered)
        waitToAwake n_registered ph
    )
  )

-- | @await@ a @Phaser@ for a specified number of phases.
awaitFor
  :: Enum p
  => Int -- ^ Number of phases to wait for
  -> Phaser p -- ^ Phaser to wait on
  -> IO ()
awaitFor 0 ph = return ()
awaitFor i ph = await ph >> awaitFor (i - 1) ph

-- | @await@ a @Phaser@ until it reaches a certain phase.
awaitUntil
  :: Enum p
  => Eq p
  => p        -- ^ Phase to wait for
  -> Phaser p -- ^ @Phaser@ to wait on
  -> IO ()
awaitUntil target_phase phaser =
  phase phaser >>= (\current_phase -> do
    when (current_phase /= target_phase) $
      await phaser >> awaitUntil target_phase phaser
    return ()
  )

-- | The "exit side" of @await@, which prevents threads from arriving at the
-- | @Phaser@ until all have successfully awoken.
-- | For internal use only!
waitToAwake
  :: Enum p
  => Int      -- ^ Number of parties registered at the time that threads awake.
  -> Phaser p -- ^ Phaser we're awaking on.
  -> IO ()
waitToAwake n_registered ph
  | n_registered <= 1 = return ()
  | otherwise = do
      takingMVar (_awoken ph) (\n_awake ->
        takingMVar (_awoken_needed ph) (\n_needed -> do
          let is_last_awaking = n_awake == n_needed - 1
          if is_last_awaking then do
            putMVar (_arrived ph)    (0)
            putMVar (_registered ph) (n_registered)
          else do
            putMVar (_awoken ph)        (n_awake + 1)
            putMVar (_awoken_needed ph) (n_needed)
         )
       )
{-# INLINE waitToAwake #-}

-- | Advance a @Phaser@ to the next phase.
-- | For internal use only!
nextPhase :: Enum p => Phaser p -> IO ()
nextPhase ph = modifyMVar_ (_phase ph) (return . succ)
{-# INLINE nextPhase #-}

-- XXX: Once we add exception safety, we'll get rid of this.
-- For the time being, it's just a little sugar that helps us "bracket"
-- parts of the code so that adding exception safety won't require any
-- dramatic restructuring.
takingMVar :: MVar a -> (a -> IO b) -> IO b
takingMVar mv and_then_do = takeMVar mv >>= and_then_do
