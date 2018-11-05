module Hashed(Hashed, hashed, unhashed, HashTag(hashTagged)) where

import Protolude

import Data.Functor.Classes
import Text.Show

class HashTag k where
  hashTagged :: k a -> a -> Int

data Hashed a = Hashed !a !Int
  deriving (Show)

instance Eq a => Eq (Hashed a) where
  Hashed v1 h1 == Hashed v2 h2 = h1 == h2 && v1 == v2

instance Ord a => Ord (Hashed a) where
  compare (Hashed v1 _) (Hashed v2 _) = compare v1 v2

instance Show1 Hashed where
  liftShowsPrec showa _ d (Hashed a _) = showParen (d > 10)
    $ showString "Hashed " . showa 11 a

instance Hashable (Hashed a) where
  hashWithSalt s (Hashed _ h) = hashWithSalt s h

unhashed :: Hashed a -> a
unhashed (Hashed x _) = x

hashed :: HashTag f => f a -> a -> Hashed a
hashed k v = Hashed v $ hashTagged k v
