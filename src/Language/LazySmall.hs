module Language.LazySmall where

data Term rep a where
  Var :: rep a -> Term rep a
  Abs :: (rep a -> Term rep b) -> Term rep (a -> b)
  App :: Term rep (a -> b) -> rep a -> Term rep b
  Let :: Term rep a -> (rep a -> Term rep b) -> Term rep b

type Program a = forall rep. Term rep a

type Value rep a = rep a -> Term rep a

-- interpret :: Term rep a -> Value a
-- interpret (
