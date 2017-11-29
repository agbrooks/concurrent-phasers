{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
module Control.Concurrent.Phaser
  ( newPhaser
  , newIntPhaser
  , phase
  , registered
  , arrived
  , await
  , awaitFor
  , awaitUntil
  , register
--  , signal
--  , lurk
--  , lurkFor
--  , lurkUntil
  , unregister
  , Phaser () )
where

import Control.Applicative     ( (<|>) )
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception ( bracketOnError
                         , onException
                         , uninterruptibleMask_ )
import Control.Monad           ( (>=>)
                               , join
                               , liftM
                               , when )
import Data.Maybe ( isNothing )

{-
TODO: Ensure that we can do `\`onExcept\` unregister` nonsense by adjusting
      the Countdown structure.
TODO (LT): Add support for tree-structured phasers to reduce lock contention.
TODO (LT): Add more documentation.
-}

type SleepQueue = [MVar ()]

{-|
A "Phaser" acts as a reusable barrier with an adjustable number of thread
that it synchronizes.
-}
data Phaser p = Phaser
  { _phase      :: !(MVar p)    -- ^ Current phase of the phaser
  , _arrived    :: !(MVar Int)  -- ^ Number of parties arrived at the phaser
  , _registered :: !(MVar Int)  -- ^ Number of parties registered at the phaser
  , _wake_queue :: MVar SleepQueue -- ^ A list of MVars to unblock when the phaser
                                   -- ^ advances.
  }

-- | Read the "phase" of the Phaser. Once all threads advance, the phase
--   increases.
phase :: Phaser p -> IO p
phase = readMVar . _phase

-- | Determine how many parties / threads are currently registered with the
--   @Phaser@.
registered :: Phaser p -> IO Int
registered = readMVar . _registered

-- | Determine how many parties have arrived at the @Phaser@.
arrived :: Phaser p -> IO Int
arrived = readMVar . _arrived

{- |
  Create a new @Phaser@. Note that a phaser may have no fewer than 0 parties
  registered.
-}
newPhaser :: Enum p
          => p -- ^ @phase@ to start in.
          -> Int -- ^ Number of parties to initially register with the @Phaser@.
          -> IO (Phaser p)
newPhaser p i =
  Phaser <$>
    newMVar p <*>
    newMVar 0 <*>
    newMVar i <*>
    newMVar []

-- | Create a new @Phaser@ which uses an Int to track @phase@, starting at phase 0.
newIntPhaser
  :: Int -- ^ Number of parties to initially register with the Phaser.
  -> IO (Phaser Int)
newIntPhaser = newPhaser 0

-- | Register a thread with a @Phaser@.
register :: Enum p => Phaser p -> IO ()
register ph = modifyMVar_ (_registered ph) (\i -> return (i+1))

{- |
  Immediately unregister from a phaser. If this would cause the registered count
  to drop below zero, then the registered count will not change. If this would
  cause all requisite parties to be considered "registered", then advance the
  phaser.
-}
unregister :: Enum p => Phaser p -> IO ()
unregister ph =
  modifyMVarMasked_ (_registered ph)
  (\n_reg ->
     modifyMVarMasked_ (_arrived ph)
       (\n_arr ->
         let
           any_registered = n_reg > 0
           is_done = n_arr > n_reg - 1
         in
           if (any_registered && is_done) then uninterruptibleMask_ $ do
             nextPhase ph
             forkIO $ awaken ph
             return 0
           else do
             return n_arr
     )
     return $ max 0 (n_reg - 1)
  )

sleepOn :: SleepQueue -> IO (SleepQueue, MVar ())
sleepOn queue = do
  block <- newEmptyMVar
  let queue' = block:queue
  return (queue', block)


-- | Wait at the phaser until all threads arrive. Once all threads have arrived,
-- | the @Phaser@ proceeds to the next phase, and all threads are unblocked.
await :: Enum p => Phaser p -> IO ()
await ph =
  -- FIXME: Put registered back before blocking, please.

  modifyMVarMasked_ (_registered ph)
  (\n_reg ->
     bracketOnError
       (takeMVar $ _arrived ph)
       (\n_arr -> tryPutMVar (_arrived ph) n_arr)
       (\n_arr ->
          let is_done = n_arr > n_reg - 1 in
            if is_done then uninterruptibleMask_ $ do
              nextPhase ph
              forkIO $ awaken ph
              putMVar (_arrived ph) 0
            else do
              block <- modifyMVar (_wake_queue ph) sleepOn
              putMVar (_arrived ph) (n_arr + 1)
              (takeMVar block) `onException` (modifyMVar (_arrived ph) (-1))
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

-- | Count the thread among those arriving at the phaser, but don't block
--   waiting for any others to arrive. A thread which signals the @Phaser@
--   should not `lurk` on the @Phaser@.
signal :: Enum p => Phaser p -> IO ()
signal ph = modifyMVar_ (_arrived ph) (\i -> return (i+1))

-- TODO: Consider adding "signalUntil" and "signalFor"

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
  block <- modifyMVar (_wake_queue ph) (sleepOn)
  takeMVar block >> return ()

-- | Like `awaitFor`, but `lurk`s rather than `await`s.
lurkFor :: Enum p => Int -> Phaser p -> IO ()
lurkFor 0 ph = return ()
lurkFor i ph = lurk ph >> lurkFor (i - 1) ph

-- | Like `awaitUntil`, but `lurk`s rather than `await`s.
lurkUntil :: Enum p => Eq p => p -> Phaser p -> IO ()
lurkUntil target_phase phaser =
  phase phaser >>= (\current_phase -> do
    when (current_phase /= target_phase) $
      lurk phaser >> lurkUntil target_phase phaser
    return ()
  )

-- | Advance a @Phaser@ to the next phase. For internal use only.
nextPhase :: Enum p => Phaser p -> IO ()
nextPhase ph = modifyMVar_ (_phase ph) (return . succ)
{-# INLINE nextPhase #-}

-- | Awaken all threads blocked on the phaser.
-- | For internal use only!
awaken :: Phaser p -> IO ()
awaken ph =
  uninterruptibleMask_ $
  modifyMVar_ (_wake_queue ph)
    (\wq -> do
       mapM ((flip putMVar) ()) wq
       return []
    )
