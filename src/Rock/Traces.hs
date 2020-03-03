{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
module Rock.Traces where

import Control.Monad.IO.Class
import Data.Constraint.Extras
import Data.Dependent.HashMap(DHashMap)
import qualified Data.Dependent.HashMap as DHashMap
import Data.Dependent.Sum
import Data.Functor.Classes
import Data.GADT.Compare
import Data.GADT.Show
import Data.Hashable
import Data.Some
import Text.Show.Deriving

data ValueDeps f dep a = ValueDeps
  { value :: !a
  , dependencies :: !(DHashMap f dep)
  }

return []

deriving instance (Show a, GShow f, Has' Show f dep) => Show (ValueDeps f dep a)

instance (GShow f, Has' Show f dep) => Show1 (ValueDeps f dep) where
  liftShowsPrec = $(makeLiftShowsPrec ''ValueDeps)

type Traces f dep = DHashMap f (ValueDeps f dep)

verifyDependencies
  :: (MonadIO m, GEq f, Has' Eq f dep)
  => (forall a'. f a' -> m a')
  -> (forall a'. f a' -> a' -> m (dep a'))
  -> ValueDeps f dep a
  -> m (Maybe a)
verifyDependencies fetch createDependencyRecord (ValueDeps value_ deps) = do
  upToDate <- allM (DHashMap.toList deps) $ \(depKey :=> dep) -> do
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
  :: (GEq f, Hashable (Some f))
  => f a
  -> a
  -> DHashMap f g
  -> Traces f g
  -> Traces f g
record k v deps
  = DHashMap.insert k
  $ ValueDeps v deps
