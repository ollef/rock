{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
module Traces where

import Protolude

import Data.Dependent.Map(DMap, GCompare, DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.GADT.Show
import Text.Show

import Hashed

data ValueDeps k v i = ValueDeps
  { value :: !(Hashed v i)
  , dependencies :: !(DMap k (Hashed v))
  }

deriving instance (ShowTag k v, Show (v i)) => Show (ValueDeps k v i)

instance (GShow k, ShowTag k v) => ShowTag k (ValueDeps k v) where
  showTaggedPrec k d (ValueDeps v deps) = showParen (d > 10)
    $ showString "ValueDeps " . showTaggedPrec k 11 v . showString " " . showsPrec 11 deps

type Traces k v = DMap k (ValueDeps k v)

verifyDependencies
  :: Monad m
  => (forall i'. k i' -> m (Hashed v i'))
  -> ValueDeps k v i
  -> m (Maybe (v i))
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
  :: (GCompare k, HashTag k v)
  => k i
  -> v i
  -> DMap k v
  -> Traces k v
  -> Traces k v
record k v deps
  = DMap.insert k
  $ ValueDeps (hashed k v)
  $ DMap.mapWithKey hashed deps
