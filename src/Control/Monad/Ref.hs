module Control.Monad.Ref where

import Control.Monad.ST
import Data.STRef

import Polysemy
import Polysemy.Fail
import Polysemy.State

import Control.Monad.Exit

data Ref k v m a where
  NewRef :: v -> Ref k v m k
  GetRef :: k -> Ref k v m v
  PutRef :: k -> v -> Ref k v m ()

makeSem ''Ref

refToST ::
  Member (Embed (ST st)) r =>
  Sem (Ref (STRef st v) v : r) a ->
  Sem r a
refToST = interpret \case
  NewRef v -> embed (newSTRef v)
  GetRef k -> embed (readSTRef k)
  PutRef k v -> embed (k `writeSTRef` v)

refToStateAndExit ::
  (Member (State t) r, Member (Exit (k, t)) r) =>
  (v -> t -> (k, t)) ->
  (k -> t -> Maybe v) ->
  (k -> v -> t -> Maybe t) ->
  Sem (Ref k v : r) a -> Sem r a
refToStateAndExit newK getK putK = interpret \case
  NewRef v -> do
    (k, t) <- gets (newK v)
    put t
    return k
  GetRef k -> do
    t <- get
    maybe (exit (k, t)) return (getK k t)
  PutRef k v -> do
    t <- get
    maybe (exit (k, t)) put (putK k v t)

-- class Monad m => MonadRef k v m where
--   newRef :: v -> m k
--   getRef :: k -> m v
--   setRef :: k -> v -> m ()

-- instance MonadRef (STRef s) (ST s) where
--   newRef = newSTRef
--   getRef = readSTRef
--   setRef = writeSTRef

-- instance (Num n, MonadRef r m) => MonadRef r (TickT n m) where
--   newRef = lift . newRef
--   getRef = lift . getRef
--   setRef r = lift . setRef r

-- instance (MonadRef r m) => MonadRef r (StateT s m) where
--   newRef = lift . newRef
--   getRef = lift . getRef
--   setRef r = lift . setRef r
