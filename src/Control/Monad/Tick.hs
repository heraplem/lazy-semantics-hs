{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Tick where

import Data.Monoid
import Control.Monad.Writer

class Monad m => MonadTick m where
  tick :: m ()

-- XXX `Monad m` constraint is necessary to prevent possible looping at runtime.
instance (Num n, Monad m, MonadWriter (Sum n) m) => MonadTick m where
  tick = tell 1

type TickT n m = WriterT (Sum n) m

runTickT :: Monad m => TickT n m a -> m (a, n)
runTickT m = do
  (a, Sum n) <- runWriterT m
  return (a, n)
