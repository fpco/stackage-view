{-# LANGUAGE RankNTypes #-}

-- | Make a queue which runs IO actions. The queue contains one
-- element and putting into it swaps if it's already populated.

module Control.Concurrent.STM.SwapQueue
    (newSwapQueue
    ,makeRunner)
    where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

-- | With the given enqueuer, make a runner.
makeRunner :: (IO a -> STM (STM (Maybe a))) -> (IO a -> IO (Maybe a))
makeRunner enqueue m = atomically (join (enqueue m))

-- | Start a swap queue.
newSwapQueue :: IO (IO a -> STM (STM (Maybe a)))
newSwapQueue =
  do tvar <- newEmptyTMVarIO
     void (forkIO (forever (do (action,signal) <- atomically (takeTMVar tvar)
                               result <- try action
                               atomically
                                 (putTMVar signal
                                           (case result of
                                              Left (SomeException err) ->
                                                Just (throw err)
                                              Right ok -> Just ok)))))
     return (enqueue tvar)

-- | Put or swap an item into the queue.
enqueue :: TMVar (IO a, TMVar (Maybe a)) -> IO a -> STM (STM (Maybe a))
enqueue queue action =
  do m <- tryTakeTMVar queue
     case m of
       Nothing -> return ()
       Just (_,signal) -> putTMVar signal Nothing
     rvar <- newEmptyTMVar
     putTMVar queue (action,rvar)
     return (readTMVar rvar)
