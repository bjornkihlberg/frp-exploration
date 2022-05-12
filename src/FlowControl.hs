{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module FlowControl where

import Control.Concurrent (MVar)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Control
import Control.Monad.Trans.Cont (ContT (ContT))

manage :: IO a -> (a -> IO b) -> ContT c IO a
manage resource = ContT . Exception.bracket resource

shortCircuitMaybe :: Monad m => r -> m (Maybe a) -> ContT r m a
shortCircuitMaybe r m = ContT \k -> maybe (pure r) k =<< m

shortCircuitEither :: Monad m => (e -> r) -> m (Either e a) -> ContT r m a
shortCircuitEither f m = ContT \k -> either (pure . f) k =<< m

continueWhen :: Monad m => Bool -> ContT () m ()
continueWhen x = ContT \k -> Control.when x (k ())

loopUntil :: Monad m => m Bool -> ContT () m ()
loopUntil mx = ContT \k -> let loop = do x <- mx; Control.unless x (k () >> loop) in loop

async :: IO a -> IO (IO a)
async program = do
  mvar :: MVar a <- Concurrent.newEmptyMVar
  Control.void $ Concurrent.forkIO $ Concurrent.putMVar mvar =<< program
  pure (Concurrent.takeMVar mvar)

channel :: IO ((a -> IO ()) -> IO (), a -> IO (), IO ())
channel = do
  mvar :: MVar (Maybe a) <- Concurrent.newEmptyMVar
  let send message =
        Concurrent.tryReadMVar mvar >>= \case
          Just Nothing -> pure ()
          _ -> Concurrent.putMVar mvar message
      receive k =
        Concurrent.tryTakeMVar mvar >>= \case
          Just (Just x) -> k x
          _ -> pure ()
  pure (receive, send . Just, send Nothing)
