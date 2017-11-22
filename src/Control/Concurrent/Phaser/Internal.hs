module Control.Concurrent.Phaser.Internal
where

import Control.Concurrent      ( forkIO )
import Control.Concurrent.MVar
import Control.Exception ( bracketOnError )
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
  , on_completion :: Int -> IO () -- ^ An effect triggered on completion of the
                                  --   countdown, passed the number of registered
                                  --   parties as an argument.
  }

-- | Set the number of parties arrived on a @Countdown@ to zero. If a number of
--   parties arrived is already present, this will block.
reset :: Countdown -> IO ()
reset c = putMVar (arrived c) 0

-- | Set the number of parties registered on a @Countdown@ to an arbitrary
--   value. Be careful using this unless it is known that the @Countdown@ is not
--   in use and that the number of registered parties is empty.
setRegistered :: Int -> Countdown -> IO ()
setRegistered r c = putMVar (registered c) r

getRegistered :: Countdown -> IO Int
getRegistered c = readMVar (registered c)

-- | Create a new @Countdown@ for 'i' parties that executes callback 'callback'.
--   'i' may not drop below zero.
newCountdown :: Int -> (Int -> IO ()) -> IO Countdown
newCountdown i callback
  = Countdown
  <$> newMVar (max i 0)
  <*> newMVar 0
  <*> return callback

-- | Create a new @Countdown@ with 'i' parties that executes callback 'callback'.
--   'i' may not drop below zero. Any attempts to arrive at the @Countdown@ will
--   block.
newDisabledCountdown :: Int -> (Int -> IO ()) -> IO Countdown
newDisabledCountdown i callback
  = Countdown
  <$> newMVar (max i 0)
  <*> newEmptyMVar
  <*> return callback

-- | Replace the action performed by a @Countdown@.
withNewAction :: Countdown -> (Int -> IO ()) -> Countdown
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
  -- FIXME: Sanity check this when you've had more sleep.
  --        What if the callback runs, then the thread is killed?
      bracketOnError
        (takeMVar (arrived c))
        (\n_arr -> putMVar (arrived c) n_arr)
        (\n_arr -> do
            let countdown_done = n_arr >= n_reg - 1
            if (countdown_done) then
              runCallback c n_reg
            else do
              putMVar (arrived c) n_arr
        )
      return $ max 0 (n_reg - 1)
  )

-- | Arrive at a @Countdown@. If we are the last party to arrive, run the
--   callback and do not replaced the 'arrived' counter.
arriveCountdown :: Countdown -> IO ()
arriveCountdown c =
  withMVar (registered c)
  (\n_reg ->
      bracketOnError
        (takeMVar (arrived c))
        (\n_arr -> putMVar (arrived c) n_arr)
        (\n_arr -> do
            if (n_arr >= n_reg - 1) then
              runCallback c n_reg
            else do
              putMVar (arrived c) (n_arr + 1)
        )
  )

-- | Run the action associated with a @Countdown@'s completion in a new thread.
--   This operation is strict.
runCallback :: Countdown -> Int -> IO ()
runCallback c num_reg = forkIO (on_completion c num_reg) >> return ()
{-# INLINABLE runCallback #-}
