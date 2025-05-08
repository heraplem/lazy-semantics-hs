module Language.LazySmall where

import Numeric.Natural
import Data.Maybe
import Control.Monad

import Control.Lens

import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Input
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Tagged

import Data.Functor.Base
import Data.Functor.Foldable

import Control.Monad.Exit
import Control.Monad.Heap.List
import Control.Monad.Ref
import Control.Monad.Tick

data Term rep
  = Var rep
  | Abs (rep -> Term rep)
  | App (Term rep) rep
  | Let (Term rep) (rep -> Term rep)

type Program = forall rep. Term rep

data TermF rep r
  = VarF rep
  | AbsF (rep -> r)
  | AppF r rep
  | LetF r (rep -> r)
  deriving (Functor)

type instance Base (Term rep) = TermF rep

instance Recursive (Term rep) where
  project (Var x) = VarF x
  project (Abs f) = AbsF f
  project (App t x) = AppF t x
  project (Let x f) = LetF x f

instance Corecursive (Term rep) where
  embed (VarF x) = Var x
  embed (AbsF f) = Abs f
  embed (AppF t x) = App t x
  embed (LetF x f) = Let x f

pp :: Member (Input rep) r => (rep -> String) -> Term rep -> Sem r String
pp proj = cata \case
  VarF x ->
    pure (proj x)
  AbsF f -> do
    x <- input
    pure "(lambda" <> pure (proj x) <> pure "." <> f x <> pure ")"
  AppF t x ->
    pure "(" <> t <> pure " " <> pure (proj x) <> pure ")"
  LetF t f -> do
    x <- input
    pure "(let " <> pure (proj x) <> pure " = " <> t <> pure " in " <> f x <> pure ")"

ppShow :: (Show rep, Member (Input rep) r) => Term rep -> Sem r String
ppShow = pp show

normalize :: ( Member (Exit ()) r
             , Member (Ref k (Term k)) r
             , Member Tick r
             ) =>
  Term k ->
  Sem r (Term k)
normalize (Var x) = do
  tick
  t <- normalize =<< getRef x
  x `putRef` t
  return t
normalize t@(Abs _) =
  return t
normalize (App t x) = do
  tick
  normalize t >>= \case
    Abs f -> normalize (f x)
    _ -> exit ()
normalize (Let t f) = do
  tick
  normalize . f =<< newRef t

normalizeListHeap ::
  Term Int ->
  Either String (Natural, (Heap (Term Int), Term Int))
normalizeListHeap
  = run
  . runFail
  . runTickSum
  . exitToFail (const "heap lookup error")
  . runState empty
  . refToHeapStateAndExit
  . exitToFail (const "app of non-abs")
  . normalize

-- normalize :: (MonadFail m, MonadTick m, MonadRef k (Term k) m) => Term k -> m (Term k)
-- normalize (Var x) = do
--   tick
--   t <- getRef x
--   t' <- normalize t
--   x `setRef` t'
--   return t'
-- normalize t@(Abs _) =
--   return t
-- normalize (App t x) = do
--   tick
--   t' <- normalize t
--   case t' of
--     Abs f -> normalize (f x)
--     _ -> fail "app of non-abs"
-- normalize (Let t f) = do
--   tick
--   x <- newRef t
--   normalize (f x)


-- normalizeFixTerm :: (MonadTick m, MonadRef r m) => FixTerm r -> m (FixTerm r)
-- normalizeFixTerm (Var x) = do
--   tick
--   In t <- getRef x
--   t' <- normalizeFixTerm t
--   x `setRef` In t'
--   return t'
-- normalizeFixTerm t@(Abs _) =
--   return t
-- normalizeFixTerm (App t x) = do
--   tick
--   t' <- normalizeFixTerm t
--   case t' of
--     Abs f -> normalizeFixTerm (f x)
--     _ -> error "app of non-abs"
-- normalizeFixTerm (Let t f) = do
--   tick
--   x <- newRef (In t)
--   normalizeFixTerm (f x)

-- pp :: (forall m. MonadRef r m => FixTerm r) -> (

-- newtype NormalizeM s a = NormalizeM
--   { getNormalizeM :: StateT Natural (TickT Natural (ST s)) a
--   }
--   deriving (Functor, Applicative, Monad)

-- runNormalizeM :: (forall s. NormalizeM s a) -> (a, Natural)
-- runNormalizeM m =
--   let ((a, _), n) = runST (runTickT $ flip runStateT 0 $ getNormalizeM m)
--   in (a, n)

-- instance MonadTick (NormalizeM s) where
--   tick = NormalizeM tick

-- data NormalizeMRef s a
--   = NormalizeMRef (STRef s a) Natural

-- instance MonadRef (NormalizeMRef s) (NormalizeM s) where
--   newRef a = NormalizeM do
--     n <- get
--     put (n + 1)
--     r <- newRef a
--     return (NormalizeMRef r n)
--   getRef (NormalizeMRef r _) = NormalizeM $ getRef r
--   setRef (NormalizeMRef r _) = NormalizeM . setRef r

-- forget :: FixTerm (NormalizeMRef s) -> Term Natural
-- forget (In t) = case t of
--   Var (NormalizeMRef _ n) -> Var n
--   Abs f -> Abs (forget . In . f . NormalizeMRef undefined)
--   App t (NormalizeMRef _ n) -> App (forget (In t)) n
--   Let t f -> Let (forget (In t)) (forget . In . f . NormalizeMRef undefined)

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

mapInput ::
  (j -> i) ->
  Sem (Input i : r) a ->
  Sem (Input j : r) a
mapInput proj = reinterpret \case
  Input -> proj <$> input

ex' :: Either String String
ex' = do
  (_, (h, t)) <- normalizeListHeap ex
  return (run . runInputList [h^.size ..] . mapInput fromJust . ppShow $ t)
