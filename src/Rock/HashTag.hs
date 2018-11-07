module Rock.HashTag where

import Protolude

class HashTag k where
  hashTagged :: k a -> a -> Int
