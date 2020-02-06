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

import Rock.HashTag

data ValueDeps f a = ValueDeps
  { value :: !a
  , dependencies :: !(DMap f (Const Int))
  }

return []

deriving instance (Show a, ShowTag f (Const Int)) => Show (ValueDeps f a)

instance ShowTag f (Const Int) => Show1 (ValueDeps f) where
  liftShowsPrec = $(makeLiftShowsPrec ''ValueDeps)

type Traces f = DMap f (ValueDeps f)

verifyDependencies
  :: (Monad m, HashTag f)
  => (forall a'. f a' -> m a')
  -> ValueDeps f a
  -> m (Maybe a)
verifyDependencies fetch (ValueDeps value_ deps) = do
  upToDate <- allM (DMap.toList deps) $ \(depKey :=> Const depHash) -> do
    depValue <- fetch depKey
    return $ hashTagged depKey depValue == depHash
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
  -> DMap f (Const Int)
  -> Traces f
  -> Traces f
record k v deps
  = DMap.insert k
  $ ValueDeps v deps
