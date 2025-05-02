{-# LANGUAGE QuantifiedConstraints, KindSignatures, ImpredicativeTypes #-}

module Language.Lazy where

import Data.Proxy
import Data.Monoid
import Data.Coerce
import Data.Kind
import Data.Type.Equality
import Data.Functor.Identity
import Control.Monad.Writer

data Ty
  = TBool
  | TList Ty

-- type family Rep (a :: Ty) :: Type where
--   Rep TBool = Bool
--   Rep (TList a) = [Rep a]

-- class Monad m => Rep (m :: Type -> Type) where
--   type Ref m :: Ty -> Type

-- type Rep = Type -> Type

-- type IsARep (rep :: Ty -> Type) = (rep TBool ~ Bool, forall (a :: Ty). rep (TList a) ~ [rep a])

-- type family CRep (a :: Ty) :: Type where
--   CRep TBool = Bool
--   CRep (TList a) = [CRep a]

data Term rep a :: Type where
  TTrue :: Term rep Bool
  TFalse :: Term rep Bool
  TNil :: Term rep [a]
  TCons :: rep a -> rep [a] -> Term rep [a]
  TFoldr :: (rep a -> rep b -> Term rep b) -> rep b -> rep [a] -> Term rep b
  TLet :: Term rep a -> (rep a -> Term rep b) -> Term rep b

interpret :: (forall rep. Term rep a) -> a
interpret t = go t where
  go :: Term Identity a -> a
  go TTrue = True
  go TFalse = False
  go TNil = []
  go (TCons x xs) = runIdentity x : runIdentity xs
  go (TFoldr f e xs) = case runIdentity xs of
    [] -> runIdentity e
    (x:xs) -> go (Identity x `f` Identity (go (TFoldr f e (Identity xs))))
  go (TLet d b) = go (b (Identity (go d)))

interpret' :: Num n => (forall rep. Term rep a) -> Writer (Sum n) a
interpret' = _
