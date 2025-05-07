module Control.Monad.Fresh where

import Polysemy
import Polysemy.State

data Fresh t m a where
  Fresh :: Fresh t m t

makeSem ''Fresh

freshToState :: Member (State t) r => (t -> t) -> Sem (Fresh t : r) a -> Sem r a
freshToState f = interpret \case
  Fresh -> do
    t <- get
    put (f t)
    return t
