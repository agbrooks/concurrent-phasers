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
import Control.Monad           ( (>=>)
                               , join
                               , liftM
                               , when )
import Data.Maybe ( isNothing )

{-
KILLED DURING TASK      -- Arrive and unregister (specified by user).
KILLED BLOCKED ON AWAIT -- Unregister. Trigger awake if last (specified by us).
KILLED AWAKING          -- Unregister. Trigger await if last (specified by us).
-}

{-
TODO: Add the above and make it work correctly.
TODO: Add support for tree-structured phasers to reduce lock contention.
TODO: Add more documentation.
-}

data RegistrationInfo = RegistrationInfo
  { _registered    :: Int
  , _unregistering :: Int
  }


{-|
A "Phaser" acts as a reusable barrier with an adjustable number of threads
that are synchronized by it.
-}
data Phaser p = Phaser
  { _phase   :: !(MVar p) -- ^ Current phase of the phaser
  , _registration   :: !(MVar RegistrationInfo) -- ^ Registration info for the phaser.
  , _arrived :: !(MVar Int)  -- ^ Number of threads arrived and registered.
  , _awoken  :: !(MVar Int)  -- ^ Number of threads awake and needed.
  }

-- | Read the "phase" of the Phaser. Once all threads advance, the phase
--   increases.
phase :: Phaser p -> IO p
phase = (readMVar . _phase)

-- | Determine how many parties / threads are currently registered with the
--   @Phaser@.
registered :: Phaser p -> IO Int
registered ph = _registered <$> readMVar (_registration ph)

-- | Determine how many parties have arrived at the @Phaser@.
arrived :: Phaser p -> IO Int
arrived ph = readMVar (_arrived ph)

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
  <*> newMVar (RegistrationInfo i 0)
  <*> newMVar 0
  <*> newEmptyMVar

-- | Create a new @Phaser@ which uses an Int to track @phase@, starting at phase 0.
newIntPhaser
  :: Int -- ^ Number of parties to initially register with the Phaser.
  -> IO (Phaser Int)
newIntPhaser = newPhaser 0

-- | Register a thread with a @Phaser@.
register :: Enum p => Phaser p -> IO ()
register ph = modifyMVar_ (_registration ph)
  (\(RegistrationInfo r u) -> return $ RegistrationInfo (r + 1) u)

-- | Arrive at a phaser and immediately unregister.
arriveAndUnregister :: Enum p => Phaser p -> IO ()
arriveAndUnregister = unregister >=> arrive

-- | Wait at the phaser until all threads arrive. Once all threads have arrived,
-- | the @Phaser@ proceeds to the next phase, and all threads are unblocked.
await :: Enum p => Phaser p -> IO ()
await = arriveThen
  (\ph n_registered ->
     when (n_registered > 1) $ waitToAwake ph
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
-- | waiting for any others to arrive.
signal :: Enum p => Phaser p -> IO ()
signal = arriveThen (\_ _ -> return ())

-- | FOR INTERNAL USE ONLY - declares intent to deregister to RegistrationInfo.
-- | Will never cause a phaser to advance.
unregister :: Phaser p -> IO (Phaser p)
unregister ph = modifyMVar_ (_registration ph)
  (\(RegistrationInfo r u) -> return $ RegistrationInfo r (u + 1))


enterAwake :: Enum p => Phaser p -> IO ()
{- TODO: Enter awake phase for the current thread. This could mean either starting the
   awake process or waiting to awake.
-}

startAwake :: Enum p => Phaser p -> IO ()
startAwake ph = undefined
{- TODO:
Happens when we're the last to arrive.
Atomically:
 1. Make reg info'.
 2. If we're the last, increment the phase.
 3. Otherwise, set the awake count.
-}

waitForAwake :: Enum p => Phaser p -> IO ()
waitForAwake = undefined
{- TODO:
-- Wait for another thread to start the awake.
-- Atomically:
--   * add 1 to awake count.
--   * if we're the last, update the registration count and make awaitable.
--   * if not, put the awake count.
-- If we encounter an async exception while awaiting:
--   * unregister, check if we're the last. If so, update registertaion count and make awaitable.
-}

-- | FOR INTERNAL USE ONLY - Arrive at the phaser, then perform a provided action,
-- | if the thread arriving is not the last.
arriveThen
  :: Enum p
  => (Phaser p -> Int -> IO ())
  -- ^ Action, taking a phaser and number of parties currently registered.
  -> Phaser p -- ^ The phaser to arrive on, also passed as an argument to the action.
  -> IO ()
arriveThen do_this ph =
  

-- | FOR INTERNAL USE ONLY - Advance a @Phaser@ to the next phase.
nextPhase :: Enum p => Phaser p -> IO ()
nextPhase ph = modifyMVar_ (_phase ph) (return . succ)
{-# INLINE nextPhase #-}

takingMVar :: MVar a -> (a -> IO b) -> IO b
takingMVar mv and_then = do
  takeMVar mv >>= and_then

-- | FOR INTERNAL USE ONLY -
--   Actually apply the intention to unregister to the RegistrationInfo.
nextUnregistering :: RegistrationInfo -> RegistrationInfo
nextUnregistering i@(RegistrationInfo r u) = RegistrationInfo (expected i) 0

-- | FOR INTERNAL USE ONLY -
--   Taking into account the number of parties that want to unregister, how
--   many will be registered once we update?
willBeRegistered :: RegistrationInfo -> Int
willBeRegistered (RegistrationInfo r u) = max 0 (r - u)

-- | FOR INTERNAL USE ONLY - Are we the last thread to arrive or awake?
isLastThread :: Int -> RegistrationInfo -> Bool
isLastThread n_already_there reg_info = (willBeRegistered reg_info) <= n_already_there - 1
