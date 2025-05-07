module Control.Monad.Exit where

import Polysemy
import Polysemy.Error
import Polysemy.Fail

-- | A generalization of the 'Error' and 'Fail' effects.  It generalizes 'Fail'
-- by allowing error types other than 'String'; it generalizes 'Error' by not
-- providing a 'catch' function.
data Exit e m a where
  Exit :: e -> Exit e m a

makeSem ''Exit

-- | Transform a 'Fail' effect into an 'Exit' effect over 'String'.
failToExit ::
  Member (Exit String) r =>
  Sem (Fail : r) a ->
  Sem r a
failToExit = interpret \case
  Fail s -> exit s

-- | Transform an 'Exit' effect into an 'Error' effect.
exitToError ::
  Member (Error e) r =>
  Sem (Exit e : r) a ->
  Sem r a
exitToError = interpret \case
  Exit e -> throw e

-- | Run an 'Exit' effect like 'ErrorT'.
runExitEither ::
  forall e r a.
  Sem (Exit e : r) a ->
  Sem r (Either e a)
runExitEither = runError . exitToError . raiseUnder

-- | Run an 'Exit' effect, throwing away any error information.
runExitMaybe ::
  Sem (Exit e : r) a ->
  Sem r (Maybe a)
runExitMaybe = fmap (either (const Nothing) Just) . runExitEither
