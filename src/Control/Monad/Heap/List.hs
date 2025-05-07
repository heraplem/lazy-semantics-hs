module Control.Monad.Heap.List where

import Control.Lens
import Polysemy
import Polysemy.State
import Control.Monad.Exit
import Control.Monad.Ref

data Heap a = Heap
  { _size :: Int
  , _elems :: [a]
  }
  deriving (Show)

makeLenses ''Heap

empty :: Heap a
empty = Heap
  { _size = 0
  , _elems = []
  }

refToHeapStateAndExit ::
  (Member (State (Heap a)) r, Member (Exit (Int, Heap a)) r) =>
  Sem (Ref Int a : r) a -> Sem r a
refToHeapStateAndExit = refToStateAndExit newK getK putK where
  newK x h = (h ^. size, h & size +~ 1 & elems |>~ x)
  getK i h = h ^? elems . ix i
  putK i x = failover (elems . ix i) (const x)
