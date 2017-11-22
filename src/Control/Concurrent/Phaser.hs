{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
module Control.Concurrent.Phaser
  ( newPhaser
  , newIntPhaser
  , phase
  , Control.Concurrent.Phaser.registered
  , Control.Concurrent.Phaser.arrived
  , await
  , awaitFor
  , awaitUntil
  , register
  , signal
  , lurk
  , lurkFor
  , lurkUntil
  , unregister
  , Phaser () )
where

import Control.Applicative     ( (<|>) )
import Control.Concurrent.MVar
import Control.Exception ( uninterruptibleMask_ )
import Control.Monad           ( (>=>)
                               , join
                               , liftM
                               , when )
import Data.Maybe ( isNothing )
import Data.IORef

import Control.Concurrent.Phaser.Internal as Internal

{-
TODO: Ensure that we can do `\`onExcept\` unregister` nonsense by adjusting
      the Countdown structure.

TODO (LT): Add support for tree-structured phasers to reduce lock contention.
TODO (LT): Add more documentation.
-}

-- | Status of the phaser -- either it's waiting for all threads to join, or it's
--   waiting to tell them all that it's OK to continue.
data PhaserStatus = Awaiting | Awaking

{-|
A "Phaser" acts as a reusable barrier with an adjustable number of thread
that it synchronizes.
-}
data Phaser p = Phaser
  { _phase      :: !(MVar p) -- ^ Current phase of the phaser
  , _status     :: !(MVar PhaserStatus)
  , _awaiting   :: IORef Countdown
  , _awaking    :: IORef Countdown
  }

awaiting = readIORef . _awaiting
awaking  = readIORef . _awaking

-- | Read the "phase" of the Phaser. Once all threads advance, the phase
--   increases.
phase :: Phaser p -> IO p
phase = readMVar . _phase

-- | Determine how many parties / threads are currently registered with the
--   @Phaser@.
registered :: Phaser p -> IO Int
registered ph =
  withMVar (_status ph)
  (\status ->
     case status of
       Awaiting -> readMVar . Internal.registered =<< (awaiting ph)
       Awaking  -> readMVar . Internal.registered =<< (awaking ph)
  )

-- | Determine how many parties have arrived at the @Phaser@.
arrived :: Phaser p -> IO Int
arrived ph = readMVar . Internal.arrived =<< (awaiting ph)

{- |
  Create a new @Phaser@. Note that a phaser may have no fewer than 0 parties
  registered.
-}
newPhaser :: Enum p
          => p -- ^ @phase@ to start in.
          -> Int -- ^ Number of parties to initially register with the @Phaser@.
          -> IO (Phaser p)
newPhaser p i = do
  -- Make both "halves" of the Phaser
  awaiting_countdown <- newIORef =<< newCountdown         i undefined
  awaking_countdown  <- newIORef =<< newDisabledCountdown i undefined

  -- Create a new phaser.
  new_phaser <- Phaser
      <$> newMVar p                 -- Use start phase
      <*> newMVar Awaiting          -- Start in "awaiting" mode
      <*> return awaiting_countdown -- Reference to awaiting countdown
      <*> return awaking_countdown  -- Reference to awaking  countdown

  -- Describe the actions that should occur when each countdown finishes.
  -- Phaser state is updated and the other countdown can begin.
  let
    -- Awaiting -> Awaking
    -- FIXME: Race condition: Can't do getRegistered if the callback triggers...
    -- FIXME: This is probably why everything deadlocks on advance
    switchToAwaking = uninterruptibleMask_ $ do
      takeMVar (_status new_phaser)                       -- Seize status lock.
      nextPhase new_phaser                                -- Advance to next phase.
      registered <- getRegistered =<< awaiting new_phaser -- Update n. registered
      (setRegistered registered)  =<< awaking new_phaser
      putMVar (_status new_phaser) Awaking                -- Mark as awaking.
      awaking new_phaser >>= reset                        -- Begin awaking.

    -- Awaking -> Awaiting
    switchToAwaiting = uninterruptibleMask_ $ do
      takeMVar (_status new_phaser)                       -- Seize status lock.
      registered <- getRegistered =<< awaking new_phaser  -- Update n. registered
      (setRegistered registered)  =<< awaiting new_phaser
      putMVar (_status new_phaser) Awaiting               -- Mark as awaiting.
      awaiting new_phaser >>= reset                       -- Begin awaiting

  modifyIORef' awaiting_countdown (`withNewAction` switchToAwaking)
  modifyIORef' awaking_countdown  (`withNewAction` switchToAwaiting)
  return $! new_phaser

-- | Create a new @Phaser@ which uses an Int to track @phase@, starting at phase 0.
newIntPhaser
  :: Int -- ^ Number of parties to initially register with the Phaser.
  -> IO (Phaser Int)
newIntPhaser = newPhaser 0

-- | Register a thread with a @Phaser@.
register :: Enum p => Phaser p -> IO ()
register ph = withMVar (_status ph)
  (\status ->
     case status of
       Awaiting -> (awaiting ph) >>= registerCountdown
       Awaking  -> (awaking  ph) >>= registerCountdown
  )

-- | Arrive at a phaser and immediately unregister.
unregister :: Enum p => Phaser p -> IO ()
unregister ph = withMVar (_status ph)
  (\status ->
     case status of
       Awaiting -> (awaiting ph) >>= unregisterArriveCountdown
       Awaking  -> (awaking  ph) >>= unregisterArriveCountdown
  )

-- | Wait at the phaser until all threads arrive. Once all threads have arrived,
-- | the @Phaser@ proceeds to the next phase, and all threads are unblocked.
await :: Enum p => Phaser p -> IO ()
await ph = do
  awaiting <- awaiting ph
  awaking  <- awaking ph
  arriveCountdown awaiting >> arriveCountdown awaking

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

-- | Count the thread among those arriving at the phaser, but don't block
--   waiting for any others to arrive. A thread which signals the @Phaser@
--   should not `lurk` on the @Phaser@.
signal :: Enum p => Phaser p -> IO ()
signal ph = arriveCountdown =<< awaiting ph

{- |
  Don't count the thread among those arriving at the phaser, but don't
  block waiting for any others to arrive. A thread that lurks on the @Phaser@
  should not `signal` on the @Phaser@.

  Be careful when using `lurk` - if the phaser advances before
  lurk is called, it may cause `lurk` to wait for a cycle after the one we
  actually care about. For this reason, it's probably safer to use `lurkUntil`
  or `lurkFor`.

-}
lurk :: Enum p => Phaser p -> IO ()
lurk ph = do
  (readMVar . Internal.arrived) =<< (awaking ph)
  return ()

-- | Like `awaitFor`, but `lurk`s rather than `arrive`s.
lurkFor :: Enum p => Int -> Phaser p -> IO ()
lurkFor 0 ph = return ()
lurkFor i ph = lurk ph >> lurkFor (i - 1) ph

-- | Like `awaitUntil`, but `lurk`s rather than `arrive`s.
lurkUntil :: Enum p => Eq p => p -> Phaser p -> IO ()
lurkUntil target_phase phaser =
  phase phaser >>= (\current_phase -> do
    when (current_phase /= target_phase) $
      lurk phaser >> lurkUntil target_phase phaser
    return ()
  )

-- | FOR INTERNAL USE ONLY - Advance a @Phaser@ to the next phase.
nextPhase :: Enum p => Phaser p -> IO ()
nextPhase ph = modifyMVar_ (_phase ph) (return . succ)
{-# INLINE nextPhase #-}
