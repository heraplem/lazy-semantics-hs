module Control.Monad.Tick where

import Data.Monoid

import Control.Lens

import Polysemy
import Polysemy.Output

data Tick m a where
  Tick :: Tick m ()

makeSem ''Tick

tickToOutput :: Member (Output o) r => o -> Sem (Tick : r) a -> Sem r a
tickToOutput o = interpret \case
  Tick -> output o

runTickSum :: forall n r a. Num n => Sem (Tick : r) a -> Sem r (n, a)
runTickSum = fmap (_1 %~ getSum) . runOutputMonoid id . tickToOutput 1 . raiseUnder

-- class Monad m => MonadTick m where
--   tick :: m ()

-- newtype TickT n m a = TickT { getTickT :: WriterT (Sum n) m a }
--   deriving (Functor, Applicative, Monad)

-- instance Num n => MonadTrans (TickT n) where
--   lift = TickT . lift

-- instance (Num n, Monad m) => MonadTick (TickT n m) where
--   tick = TickT (tell 1)

-- instance MonadTick m => MonadTick (StateT s m) where
--   tick = lift tick

-- runTickT :: Monad m => TickT n m a -> m (a, n)
-- runTickT (TickT m) = do
--   (a, Sum n) <- runWriterT m
--   return (a, n)
