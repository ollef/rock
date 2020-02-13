{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
module Rock.Traces where

import Protolude

import Data.Dependent.Map(DMap, GCompare, DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Classes
import Text.Show.Deriving

data ValueDeps f dep a = ValueDeps
  { value :: !a
  , dependencies :: !(DMap f dep)
  }

return []

deriving instance (Show a, ShowTag f dep) => Show (ValueDeps f dep a)

instance ShowTag f dep => Show1 (ValueDeps f dep) where
  liftShowsPrec = $(makeLiftShowsPrec ''ValueDeps)

type Traces f dep = DMap f (ValueDeps f dep)

verifyDependencies
  :: (MonadIO m, EqTag f dep)
  => (forall a'. f a' -> m a')
  -> (forall a'. f a' -> a' -> m (dep a'))
  -> ValueDeps f dep a
  -> m (Maybe a)
verifyDependencies fetch createDependencyRecord (ValueDeps value_ deps) = do
  upToDate <- allM (DMap.toList deps) $ \(depKey :=> dep) -> do
    depValue <- fetch depKey
    newDep <- createDependencyRecord depKey depValue
    return $ eqTagged depKey depKey dep newDep
  return $ if upToDate
    then Just value_
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
  :: GCompare f
  => f a
  -> a
  -> DMap f g
  -> Traces f g
  -> Traces f g
record k v deps
  = DMap.insert k
  $ ValueDeps v deps
