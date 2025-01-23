module Lib (forceElems) where

data Id a = MkId {runId :: a}

instance Functor Id where
  fmap f (MkId a) = MkId (f $! a)

instance Applicative Id where
  pure = MkId
  MkId f <*> i = fmap f i

forceElems :: (Traversable t) => t a -> t a
forceElems = runId . traverse MkId
