{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
module Hashed(Hashed, hashed, unhashed, HashTag(hashTagged)) where

import Protolude

import Text.Show

import Data.Dependent.Sum

class HashTag k v where
  hashTagged :: k i -> v i -> Int

data Hashed v i = Hashed !(v i) !Int
  deriving (Show)

instance Eq (v i) => Eq (Hashed v i) where
  Hashed v1 h1 == Hashed v2 h2 = h1 == h2 && v1 == v2

instance Ord (v i) => Ord (Hashed v i) where
  compare (Hashed v1 _) (Hashed v2 _) = compare v1 v2

instance Hashable (Hashed v i) where
  hashWithSalt s (Hashed _ h) = hashWithSalt s h

instance ShowTag k v => ShowTag k (Hashed v) where
  showTaggedPrec k d (Hashed v _) = showParen (d > 10)
    $ showString "Hashed " . showTaggedPrec k 11 v

unhashed :: Hashed v i -> v i
unhashed (Hashed x _) = x

hashed :: HashTag k v => k i -> v i -> Hashed v i
hashed k v = Hashed v $ hashTagged k v
