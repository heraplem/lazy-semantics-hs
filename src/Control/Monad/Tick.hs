module Control.Monad.Tick where

import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State

class Monad m => MonadTick m where
  tick :: m ()

newtype TickT n m a = TickT { getTickT :: WriterT (Sum n) m a }
  deriving (Functor, Applicative, Monad)

instance Num n => MonadTrans (TickT n) where
  lift = TickT . lift

instance (Num n, Monad m) => MonadTick (TickT n m) where
  tick = TickT (tell 1)

instance MonadTick m => MonadTick (StateT s m) where
  tick = lift tick

runTickT :: Monad m => TickT n m a -> m (a, n)
runTickT (TickT m) = do
  (a, Sum n) <- runWriterT m
  return (a, n)
