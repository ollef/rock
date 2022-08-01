{-# language GADTs #-}
module Subquery where

import Rock

data Subquery a where
  SQ :: Subquery Int

subrules :: Rules Subquery
subrules SQ = pure 610

data Query a where
  Subquery :: Subquery a -> Query a
  Query :: Query Int

rules :: Rules Query
rules (Subquery sq) = transFetch (fetch . Subquery) $ subrules sq
rules Query = pure 611
