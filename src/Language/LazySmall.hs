{-# LANGUAGE RebindableSyntax, ImplicitPrelude #-}

module Language.LazySmall where

import Numeric.Natural
import Control.Monad.ST
import Data.STRef
import Control.Monad.State
import Control.Monad.Tick

data Term rep where
  Var :: rep -> Term rep
  Abs :: (rep -> Term rep) -> Term rep
  App :: Term rep -> rep -> Term rep
  Let :: Term rep -> (rep -> Term rep) -> Term rep

type Program = forall rep. Term rep

newtype FixTerm s = In { out :: Term (STRef s (FixTerm s)) }

normalize' :: FixTerm s -> TickT Natural (ST s) (FixTerm s)
normalize' (In t) = case t of
  Var x -> do
    tick
    t <- lift (readSTRef x)
    t' <- normalize' t
    lift (x `writeSTRef` t')
    return t'
  App t x -> do
    tick
    In t' <- normalize' (In t)
    case t' of
      Abs f -> normalize' (In (f x))
      _ -> error "app of non-abs"
  Let t f -> do
    tick
    x <- lift (newSTRef (In t))
    normalize' (In (f x))
  t -> return (In t)  

count :: Program -> Natural
count t = runST (snd <$> (runTickT . normalize' . In $ t))

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
    return = Var
    (>>=) = Let
