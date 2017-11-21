module Control.Concurrent.Phaser.Internal
where

import Control.Concurrent      ( forkIO )
import Control.Concurrent.MVar
import Control.Monad           ( when )

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

-- | Set the number of parties arrived on a @Countdown@ to zero. If a number of
--   parties arrived is already present, this will block.
reset :: Countdown -> IO ()
reset c = putMVar (arrived c) 0

-- | Create a new @Countdown@ for 'i' parties that executes callback 'callback'.
--   'i' may not drop below zero.
newCountdown :: Int -> IO () -> IO Countdown
newCountdown i callback
  = Countdown
  <$> newMVar (max i 0)
  <*> newMVar 0
  <*> return callback

-- | Create a new @Countdown@ with 'i' parties that executes callback 'callback'.
--   'i' may not drop below zero. Any attempts to arrive at the @Countdown@ will
--   block.
newDisabledCountdown :: Int -> IO () -> IO Countdown
newDisabledCountdown i callback
  = Countdown
  <$> newMVar (max i 0)
  <*> newEmptyMVar
  <*> return callback

-- | Replace the action performed by a Countdown
withNewAction :: Countdown -> IO () -> Countdown
withNewAction (Countdown r a _) f = Countdown r a f

-- | Register a new party at a @Countdown@.
registerCountdown :: Countdown -> IO ()
registerCountdown c = modifyMVar_ (registered c)
  (\r -> return $ r + 1)

-- | Arrive, but unregister a party at a @Countdown@. The registration count may
--   not go below zero.
unregisterArriveCountdown :: Countdown -> IO ()
unregisterArriveCountdown c =
  modifyMVar_ (registered c)
  (\n_reg -> do
      withMVar (arrived c)
       (\n_arr -> do
          when (n_reg - 1 <= n_arr) $
            runCallback c
       )
      return $ max 0 (n_reg - 1)
  )

-- | Arrive at a @Countdown@. If we are the last party to arrive, run the
--   callback and do not replaced the 'arrived' counter.
arriveCountdown :: Countdown -> IO ()
arriveCountdown c =
  withMVar (registered c)
  (\n_reg ->
      modifyMVar_ (arrived c)
      (\n_arr -> do
          when (n_arr >= n_reg - 1) $
            runCallback c
          return (n_arr + 1)
      )
  )

-- | Run the action associated with a @Countdown@'s completion in a new thread.
--   This operation is strict.
runCallback :: Countdown -> IO ()
runCallback c = seq (forkIO $ on_completion c) (return ())
{-# INLINABLE runCallback #-}
