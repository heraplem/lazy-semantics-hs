{-# LANGUAGE QuantifiedConstraints, KindSignatures, ImpredicativeTypes #-}

module Language.Lazy where

-- import Data.Proxy
-- import Data.Monoid
-- import Data.Coerce
-- import Data.Kind
-- import Data.Type.Equality
-- import Data.Functor.Identity
-- import Control.Monad.Writer
-- import Control.Monad.State
-- import Numeric.Natural

-- data Term rep a where
--   TTrue :: Term rep Bool
--   TFalse :: Term rep Bool
--   TIf :: Term rep Bool -> Term rep a -> Term rep a -> Term rep a
--   TZero :: Term rep Natural
--   TSucc :: rep Natural -> Term rep Natural
--   TCaseN :: Term rep a -> (rep Natural -> Term rep a) -> rep Natural -> Term rep a
--   TIterate :: (rep a -> Term rep a) -> rep a -> rep Natural -> Term rep a
--   TNil :: Term rep [a]
--   TCons :: rep a -> rep [a] -> Term rep [a]
--   TCaseL :: Term rep b -> (rep a -> rep [a] -> Term rep b) -> rep [a] -> Term rep b
--   TFoldr :: (rep a -> rep b -> Term rep b) -> rep b -> rep [a] -> Term rep b
--   TVar :: rep a -> Term rep a
--   TLet :: Term rep a -> (rep a -> Term rep b) -> Term rep b

-- -- Terms not (generally) in a-normal form.
-- data NATerm rep a where
--   NATTrue :: NATerm rep Bool
--   NATFalse :: NATerm rep Bool
--   NATIf :: NATerm rep Bool -> NATerm rep a -> NATerm rep a -> NATerm rep a
--   NATZero :: NATerm rep Natural
--   NATSucc :: NATerm rep Natural -> NATerm rep Natural
--   NATCaseN :: NATerm rep a -> (rep Natural -> NATerm rep a) -> NATerm rep Natural -> NATerm rep a
--   NATIterate :: (rep a -> NATerm rep a) -> NATerm rep a -> NATerm rep Natural -> NATerm rep a
--   NATNil :: NATerm rep [a]
--   NATCons :: NATerm rep a -> NATerm rep [a] -> NATerm rep [a]
--   NATCaseL :: NATerm rep b -> (rep a -> rep [a] -> NATerm rep b) -> NATerm rep [a] -> NATerm rep b
--   NATFoldr :: (rep a -> rep b -> NATerm rep b) -> NATerm rep b -> NATerm rep [a] -> NATerm rep b
--   NATVar :: rep a -> NATerm rep a
--   NATLet :: NATerm rep a -> (rep a -> NATerm rep b) -> NATerm rep b

-- -- Convert to a-normal form.
-- anormalize :: NATerm rep a -> Term rep a
-- anormalize NATTrue = TTrue
-- anormalize NATFalse = TFalse
-- anormalize (NATIf c t f) = TIf (anormalize c) (anormalize t) (anormalize f)
-- anormalize NATZero = TZero
-- anormalize (NATSucc n) = TLet (anormalize n) TSucc
-- anormalize (NATCaseN z s n) =
--   TLet (anormalize n) \n ->
--   TCaseN (anormalize z) (\n -> anormalize (s n)) n
-- anormalize (NATIterate f n e) =
--   TLet (anormalize n) \x ->
--   TLet (anormalize e) \e ->
--   TIterate (\x -> anormalize (f x)) x e
-- anormalize NATNil = TNil
-- anormalize (NATCons x xs) =
--   TLet (anormalize x) \x ->
--   TLet (anormalize xs) \xs ->
--   TCons x xs
-- anormalize (NATCaseL n c xs) =
--   TLet (anormalize xs) \xs ->
--   TCaseL (anormalize n) (\x xs -> anormalize (c x xs)) xs
-- anormalize (NATFoldr f e xs) =
--   TLet (anormalize e) \e ->
--   TLet (anormalize xs) \xs ->
--   TFoldr (\x e -> anormalize (x `f` e)) e xs
-- anormalize (NATVar x) = TVar x
-- anormalize (NATLet d b) =
--   TLet (anormalize d) \d ->
--   anormalize (b d)

-- tcons :: Term rep a -> Term rep [a] -> Term rep [a]
-- tcons x xs = TLet x (\x -> TLet xs (\xs -> TCons x xs))

-- ttail :: rep [a] -> Term rep [a]
-- ttail = TCaseL TNil (\_ xs -> TVar xs)

-- interpret :: (forall rep. Term rep a) -> a
-- interpret t = go t where
--   go :: Term Identity a -> a
--   go TTrue = True
--   go TFalse = False
--   go TNil = []
--   go (TCons x xs) = runIdentity x : runIdentity xs
--   go (TFoldr f e xs) = case runIdentity xs of
--     [] -> runIdentity e
--     (x:xs) -> go (Identity x `f` Identity (go (TFoldr f e (Identity xs))))
--   go (TLet d b) = go (b (Identity (go d)))

-- data Cell a = Thunk (Term Cell a) | Box a

-- class Monad m => MonadTick m where
--   tick :: m ()

-- interpret' :: MonadTick m => (forall rep. Term rep a) -> m a
-- interpret' t = _ where
--   go :: (forall a. MonadState (Term Cell a) m, MonadTick m) => Term Cell a -> m a
--   go t = do
--     tick
--     case t of
--       TFalse -> return False
--       TTrue -> return True
--       -- TIf c t f -> go c >>= \case
--       --   True -> go t
--       --   False -> go f
--       -- TZero -> return 0
--       -- TSucc n -> return (runIdentity n)
--       -- TCaseN e f n -> case runIdentity n of
--       --   0 -> go e
--       --   n -> go (f (Identity (n - 1)))
--       -- TIterate f e n -> case runIdentity n of
--       --   0 -> return (runIdentity e)
--       --   n -> do
--       --     a <- Identity <$> go (TIterate f e (Identity (n - 1)))
--       --     go (f a)
--       -- TNil -> return []
--       -- TCons x xs -> return (runIdentity x : runIdentity xs)
--       -- TCaseL e f xs -> case runIdentity xs of
--       --   [] -> go e
--       --   (x:xs) -> go (f (Identity x) (Identity xs))
--       -- TFoldr f e xs -> case runIdentity xs of
--       --   [] -> return (runIdentity e)
--       --   (x:xs) -> do
--       --     b <- Identity <$> go (TFoldr f e (Identity xs))
--       --     go (Identity x `f` b)
--       TVar x -> case x of
--         Box x -> return x
--         Thunk t -> do
--           x <- go t
--           put (TVar (Box x))
--           return x
--       TLet d b -> go (b (Thunk d))

-- instance Num n => MonadTick (Writer (Sum n)) where
--   tick = tell 1

-- interpret'' :: (forall rep. Term rep a) -> (a, Int)
-- interpret'' t = (x, getSum n) where
--   (x, n) = runWriter (interpret' t)
