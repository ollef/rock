{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
module Traces where

import Protolude

import Data.Dependent.Map(DMap, GCompare, DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Classes
import Text.Show.Deriving

import Hashed

data ValueDeps f a = ValueDeps
  { value :: !(Hashed a)
  , dependencies :: !(DMap f Hashed)
  }

return []

deriving instance (Show a, ShowTag f Hashed) => Show (ValueDeps f a)

instance ShowTag f Hashed => Show1 (ValueDeps f) where
  liftShowsPrec = $(makeLiftShowsPrec ''ValueDeps)

type Traces f = DMap f (ValueDeps f)

verifyDependencies
  :: Monad m
  => (forall a'. f a' -> m (Hashed a'))
  -> ValueDeps f a
  -> m (Maybe a)
verifyDependencies fetchHash (ValueDeps hashedValue deps) = do
  upToDate <- allM (DMap.toList deps) $ \(depKey :=> depValue) -> do
    depValue' <- fetchHash depKey
    return $ hash depValue == hash depValue'
  return $ if upToDate
    then Just $ unhashed hashedValue
    else Nothing
  where
    allM :: Monad m => [a] -> (a -> m Bool) -> m Bool
    allM [] _ = return True
    allM (x:xs) p = do
      b <- p x
      if b then
        allM xs p
      else
        return False

record
  :: (GCompare f, HashTag f)
  => f a
  -> a
  -> DMap f Identity
  -> Traces f
  -> Traces f
record k v deps
  = DMap.insert k
  $ ValueDeps (hashed k v)
  $ DMap.mapWithKey (\k' (Identity v') -> hashed k' v') deps
