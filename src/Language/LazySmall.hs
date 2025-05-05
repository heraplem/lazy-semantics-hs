{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.LazySmall where

import Numeric.Natural
import Data.Functor.Const
import Control.Monad.ST
import Data.STRef
import Control.Monad.State

import Control.Monad.Ref
import Control.Monad.Tick

data Term rep where
  Var :: rep -> Term rep
  Abs :: (rep -> Term rep) -> Term rep
  App :: Term rep -> rep -> Term rep
  Let :: Term rep -> (rep -> Term rep) -> Term rep

pp :: Monad m => m String -> (rep -> String) -> Term rep -> m String
pp m s = go where
  go (Var x) = return (s x)
  go (Abs f) = do
    x <- m


type Program = forall rep. Term rep

newtype FixTerm r = In { out :: Term (r (FixTerm r)) }

normalizeFixTerm :: (MonadTick m, MonadRef r m) => FixTerm r -> m (FixTerm r)
normalizeFixTerm (In t) = case t of
  Var x -> do
    tick
    t <- getRef x
    t' <- normalizeFixTerm t
    setRef x t'
    return t'
  App t x -> do
    tick
    In t' <- normalizeFixTerm (In t)
    case t' of
      Abs f -> normalizeFixTerm (In (f x))
      _ -> error "app of non-abs"
  Let t f -> do
    tick
    x <- newRef (In t)
    normalizeFixTerm (In (f x))
  t -> return (In t)

newtype NormalizeM s a = NormalizeM
  { getNormalizeM :: StateT Natural (TickT Natural (ST s)) a
  }
  deriving (Functor, Applicative, Monad)

runNormalizeM :: (forall s. NormalizeM s a) -> (a, Natural)
runNormalizeM m =
  let ((a, _), n) = runST (runTickT $ flip runStateT 0 $ getNormalizeM m)
  in (a, n)

instance MonadTick (NormalizeM s) where
  tick = NormalizeM tick

data NormalizeMRef s a
  = NormalizeMRef (STRef s a) Natural

instance MonadRef (NormalizeMRef s) (NormalizeM s) where
  newRef a = NormalizeM do
    n <- get
    put (n + 1)
    r <- newRef a
    return (NormalizeMRef r n)
  getRef (NormalizeMRef r _) = NormalizeM $ getRef r
  setRef (NormalizeMRef r _) = NormalizeM . setRef r

forget :: FixTerm (NormalizeMRef s) -> Term Natural
forget (In t) = case t of
  Var (NormalizeMRef _ n) -> Var n
  Abs f -> Abs (forget . In . f . NormalizeMRef undefined)
  App t (NormalizeMRef _ n) -> App (forget (In t)) n
  Let t f -> Let (forget (In t)) (forget . In . f . NormalizeMRef undefined)

ex :: Program
ex = do
  zero <- Abs \z -> Abs \s -> Var z
  succ <- Abs \n -> Abs \z -> Abs \s -> do
    x <- Var n `App` z `App` s
    Var s `App` x
  plus <- Abs \m -> Abs \n ->
    Var m `App` n `App` succ
  two <- do
    one <- Var succ `App` zero
    Var succ `App` one
  two' <- do
    one <- Var succ `App` zero
    Var succ `App` one
  Var plus `App` two `App` two
  where
    (>>=) = Let

ex' :: Term Natural
ex' = fst $ runNormalizeM (forget <$> (normalizeFixTerm (In (ex))))
