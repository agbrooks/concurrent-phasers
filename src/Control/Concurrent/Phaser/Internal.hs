module Control.Concurrent.Phaser.Internal
where

import Control.Concurrent.MVar


{- |
  A one-shot @Countdown@ amongst threads.
  Several parties (threads) register/unregister at the @Countdown@. When the number
  of parties registered arrives, an action in IO occurs.

  A @Countdown@ that has completed will cause any threads that arrive to block.
  If desired, it may be re-used by resetting _arrived.

  The @Countdown@ serves as a convenient 'building block' for the Phaser. At
  present, it isn't really intended for general-purpose use.
-}
data Countdown = Countdown
  { registered  :: MVar Int -- ^ Number of parties registered. Should not drop
                             --   below zero.
  , arrived     :: MVar Int -- ^ Number of parties arrived. This value is not
                               --   reset unless reset explicitly by the
                               --  on_completion action.
  , on_completion :: IO () -- ^ An effect triggered on completion of the
                            --   countdown.
  }

-- | Create a new @Countdown@ for 'i' parties that executes callback 'callback'.
--   'i' may not drop below zero.
newCountdown :: Int -> IO () -> IO Countdown
newCountdown i callback = newDisabledCountdown i callback >>= reset

-- | Create a new @Countdown@ with 'i' parties that executes callback 'callback'.
--   'i' may not drop below zero. Any attempts to arrive at the @Countdown@ will
--   block.
newDisabledCountdown :: Int -> IO () -> IO Countdown
newDisabledCountdown i callback
  = Countdown
  <$> newMVar (max i 0)
  <*> newEmptyMVar
  <*> return callback

-- | "Reset" a Countdown, setting the number of arrived parties to zero.
reset :: Countdown -> IO Countdown
reset c = putMVar (arrived c) 0 >> return c

-- | Register a new party at a @Countdown@.
registerCountdown :: Countdown -> IO ()
registerCountdown c = modifyMVar_ (registered c)
  (\r -> return $ r + 1)

-- | Arrive, but unregister a party at a @Countdown@. The registration count may
--   not go below zero.
unregisterArriveCountdown :: Countdown -> IO ()
unregisterArriveCountdown c = modifyMVar_ (registered c)
  (\r -> return $ max 0 (r - 1))
  -- FIXME: Should determine whether or not the countdown should advance.

-- XXX: Make sure it's clear that the value taken is NOT restored...
arriveCountdown :: Countdown -> IO ()
arriveCountdown c = undefined

-- | Run the action associated with a @Countdown@.
--   This operation is strict.
runCallback :: Countdown -> IO ()
runCallback c = seq (on_completion c) (return ())
{-# INLINABLE runCallback #-}
