{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
module Control.Concurrent.Phaser
  ( newPhaser
  , newIntPhaser
  , arrived
  , await
  , awaitFor
  , awaitUntil
  , phase
  , registered
  , register
  , signal
  , unregister
  , Phaser () )
where

import Control.Applicative     ( (<|>) )
import Control.Concurrent.MVar
import Control.Exception.Base
import Control.Monad           ( join
                               , liftM
                               , when )
import Data.Maybe ( isNothing )

{-
FIXME:
A user may want to do something like
(operationWithPhaser phaser) `onExcept` (unregister phaser),
which seems pretty sane.

HOWEVER --
If the thread has *already* arrived at the phaser, then unregistering could
cause the thread to 'double-arrive' at the phaser... :/

-}

{-
TODO: Add support for tree-structured phasers to reduce lock contention.
TODO: Add more documentation.
TODO: Don't import everything from imported modules.
-}

{-|
A "Phaser" acts as a reusable barrier with an adjustable number of threads
that are synchronized by it.
-}
data Phaser p = Phaser
  { _phase :: !(MVar p) -- ^ Phase we're currently on
  , _arrived :: !(MVar ThreadQuota) -- ^ Number of threads arrived and registered.
  , _awoken :: !(MVar ThreadQuota)  -- ^ Number of threads awake and needed.
  }

{- |
This is just a simple container type used internally. In this Phaser
implementation, there are a few operations which need a "present" number of
threads to be the "needed" number of threads before proceeding. For example,
when threads @await@ a @Phaser@, we won't proceed to the next phase until the
number registered meet at the @Phaser@.

A @ThreadQuota@ is nothing more than a convenient way to express the notion of
"having" a certain number of threads and "needing" a certain number of threads.
-}
data ThreadQuota =
  ThreadQuota { present :: !Int
              , needed  :: !Int }

-- | A convenient alias for the ThreadQuota.
--   `3 \`outOf\` 6` is prettier than `ThreadQuota 3 6`, after all.
outOf = ThreadQuota

isDone :: ThreadQuota -> Bool
isDone (ThreadQuota i j) = i == j

-- | Read the "phase" of the Phaser. Once all threads advance, the phase
--   increases.
phase :: Phaser p -> IO p
phase = (readMVar . _phase)

-- | Determine how many parties / threads are currently registered with the
--   @Phaser@.
registered :: Phaser p -> IO Int
registered ph = needed <$> readMVar (_arrived ph)

-- | Determine how many parties have arrived at the @Phaser@.
arrived :: Phaser p -> IO Int
arrived ph = present <$> readMVar (_arrived ph)

{- |
  Create a new @Phaser@. Note that a phaser may have no fewer than 0 parties
  registered.

  NB: A phaser with zero parties registered behaves like a phaser with
  one party registered.
-}
newPhaser :: Enum p
          => p -- ^ @phase@ to start in.
          -> Int -- ^ Number of parties to initially register with the @Phaser@.
          -> IO (Phaser p)
newPhaser p i = Phaser
  <$> newMVar p
  <*> newMVar (0 `outOf` (max 0 i))
  <*> newEmptyMVar

-- | Create a new @Phaser@ which uses an Int to track @phase@, starting at phase 0.
newIntPhaser
  :: Int -- ^ Number of parties to initially register with the Phaser.
  -> IO (Phaser Int)
newIntPhaser = newPhaser 0

-- | Register a thread with a @Phaser@.
register :: Enum p => Phaser p -> IO ()
register ph = modifyMVar_ (_arrived ph)
  (\(ThreadQuota i j) -> return $ i `outOf` (j + 1))

{- |
  Unregister a thread from a @Phaser@. Do not permit the number of registered
  threads to drop below zero. If the thread unregistering would be the last to
  arrive, then advance all threads waiting on the @Phaser@.

  @unregister@ is uninterruptable, and can be used to safely unregister a process
  from a @Phaser@ if it encounters an exception. For example,
  `(operationWithPhaser phaser) \`onExcept\` (unregister phaser)`.
-}
unregister :: Enum p => Phaser p -> IO ()
unregister ph = uninterruptibleMask_ (_unregister ph) where
  _unregister ph = do
    successful <-  tryUnregisterSleeping ph
               <|> tryUnregisterAwaking  ph
    when (not successful) (_unregister ph)
      where

      tryUnregisterAwaking ph = do
        q <- tryTakeMVar (_awoken ph)
        case q of
          Just (ThreadQuota n_awoken n_exp) -> do
            let n_exp' = max 0 (n_exp - 1)
            if | n_awoken >= n_exp' -> undefined --TODO
               | otherwise -> putMVar (_awoken ph) (ThreadQuota n_awoken n_exp')
            return True
          _ -> return False
      {-# INLINE tryUnregisterAwaking #-}

      tryUnregisterSleeping ph = do
        q <- tryTakeMVar (_arrived ph)
        case q of
            Just (ThreadQuota n_arr n_reg) -> do
              let n_reg' = max 0 (n_reg - 1)
              if | n_arr >= n_reg' -> advance n_reg' ph
                 | otherwise -> putMVar (_arrived ph) (ThreadQuota n_arr n_reg')
              return True
            _ -> return False
      {-# INLINE tryUnregisterSleeping #-}

{-
unregister ph =
  bracketOnError (takeMVar $ _arrived ph)
    (putMVar (_arrived ph))
    (\(ThreadQuota n_arr n_reg) -> do
      let n_reg' = max 0 (n_reg - 1)
      if | n_arr >= n_reg' -> advance n_reg' ph
         | otherwise -> putMVar (_arrived ph) (ThreadQuota n_arr n_reg')
    )
-}

-- | Advance a @Phaser@ to the next phase, once all threads have arrived,
-- | and initialize the "awoken" counter to start waking up blocked threads.
-- | To avoid entering undefined state, 'advance' blocks asynchronous
-- | exceptions.
-- | For internal use only!
advance :: Enum p => Int -> Phaser p -> IO ()
advance n_registered ph = mask_ $ do
  if | n_registered <= 1 -> nextPhase ph >> reset
     | otherwise -> do
         nextPhase ph
         putMVar (_awoken ph) (ThreadQuota 1 n_registered)
  where
    reset = putMVar (_arrived ph) (ThreadQuota 0 n_registered)

-- | Wait at the phaser until all threads arrive. Once all threads have arrived,
-- | the @Phaser@ proceeds to the next phase, and all threads are unblocked.
await :: Enum p => Phaser p -> IO ()
await = arriveThen
  (\ph n_registered ->
     when (n_registered > 1) $ waitToAwake ph
  )

-- | Count the thread among those arriving at the phaser, but don't block
-- | waiting for any others to arrive.
signal :: Enum p => Phaser p -> IO ()
signal = arriveThen (\_ _ -> return ())

-- | Arrive at the phaser, then perform a provided action, if the thread arriving
--   is not the last.
--   For internal use only!
arriveThen
  :: Enum p
  => (Phaser p -> Int -> IO ())
  -- ^ Action, taking a phaser and number of parties currently registered.
  -> Phaser p -- ^ The phaser to arrive on, also passed as an argument to the action.
  -> IO ()
arriveThen do_this ph =
  takingMVar (_arrived ph) (\(ThreadQuota n_arr n_reg) -> do
    let is_last_arriving = n_arr >= n_reg - 1
    if is_last_arriving then do
      advance n_reg ph
    else do
      putMVar (_arrived ph) (ThreadQuota (n_arr + 1) n_reg)
      do_this ph n_reg
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
  => Phaser p -- ^ Phaser we're awaking on.
  -> IO ()
waitToAwake ph = do
      ThreadQuota awoken n_registered <- takeMVar $ _awoken ph
      mask_ $ do
        let is_last_awaking = awoken == n_registered - 1
        if | is_last_awaking -> putMVar (_arrived ph) (0 `outOf` n_registered)
           | otherwise -> putMVar (_awoken ph) ((awoken + 1) `outOf` n_registered)
{-# INLINE waitToAwake #-}

-- | Advance a @Phaser@ to the next phase.
-- | For internal use only!
nextPhase :: Enum p => Phaser p -> IO ()
nextPhase ph = modifyMVar_ (_phase ph) (return . succ)
{-# INLINE nextPhase #-}

takingMVar :: MVar a -> (a -> IO b) -> IO b
takingMVar mv and_then = do
  takeMVar mv >>= and_then
