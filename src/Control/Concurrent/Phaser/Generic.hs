{-# LANGUAGE KindSignatures #-}
module Control.Concurrent.Phaser.Generic
where

import Control.Monad ( when )

{- |Mode in which a task uses a @Phaser@.
    A task in `Wait` mode on a @Phaser@ will be blocked when trying to exit the
    @Phaser@ until all tasks in `Signal` mode have tried to leave.
    `SignalWait` combines `Signal` and `Wait` modes.
-}
data PhaserMode = Signal | Wait | SignalWait

-- | Does a given mode indicate that we should signal?
modeSignals :: PhaserMode -> Bool
modeSignals Wait = False
modeSignals _    = True

-- | Does a given mode indicate that we should wait?
modeWaits :: PhaserMode -> Bool
modeWaits Signal = False
modeWaits _      = True

class Phaser (r :: * -> *) where
  newPhaser     :: Enum p => p -> Int -> IO (r p)
  enterInMode   :: Enum p => r p -> PhaserMode -> IO ()
  wait          :: Enum p => r p -> IO ()
  signal        :: Enum p => r p -> IO ()
  batchRegister :: r p -> Int -> IO ()
  phase         :: r p -> IO p

-- | Create a new phaser with initial phase 0.
newIntPhaser :: (Phaser r) => Int -> IO (r Int)
newIntPhaser parties = newPhaser 0 parties

-- | Tell a @Phaser@ to expect one more party when its next phase begins.
register :: Phaser r => r p -> IO ()
register p = batchRegister p 1

-- | Tell a @Phaser@ to expect one fewer party when its next phase begins.
unregister :: Phaser r => r p -> IO ()
unregister p = batchUnregister p 1

-- | Tell a @Phaser@ to unregister several parties at once.
batchUnregister :: Phaser r => r p -> Int -> IO ()
batchUnregister p i = batchRegister p (-i)

{- |Synchronize a task with a @Phaser@.
    Caller is responsible for graceful unregistration in the event of an
    asynchronous exception.
-}
runPhased
  :: (Enum p, Phaser r)
  => r p        -- ^@Phaser in question
  -> PhaserMode -- ^Mode the thread will use when synchronizing on the @Phaser@
  -> IO a       -- ^Task performed
  -> IO a
runPhased p m action = do
  p `enterInMode` m
  result <- action
  when (modeSignals m) $ signal p
  when (modeWaits   m) $ wait p
  return result

{- |Synchronize a task across multiple @Phaser@s in multiple @PhaserMode@s.
    The @Phaser@/@PhaserMode@ pairs may be specified in any order. However,
    all @Phaser@s must have the same phase type.
-}
runMultiPhased :: (Enum p, Phaser r) => [(r p, PhaserMode)] -> IO a -> IO a
runMultiPhased [] action = action
runMultiPhased pairs action =
  let signal_pairs = filter (\(_, m) -> modeSignals m) pairs
      wait_pairs   = filter (\(_, m) -> modeWaits   m) pairs
  in do
    mapM_ (uncurry enterInMode) pairs
    result <- action
    mapM_ (signal . fst) signal_pairs
    mapM_ (wait   . fst) wait_pairs
    return result
