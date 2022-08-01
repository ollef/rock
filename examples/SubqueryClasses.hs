{-# language GADTs #-}
module SubqueryClasses where

import Rock

data Subquery1 a where
  SQ1 :: Subquery1 Int

data Subquery2 a where
  SQ2 :: Subquery2 Int

class HasSubquery1 q where
  subquery1 :: Subquery1 a -> q a

class HasSubquery2 q where
  subquery2 :: Subquery2 a -> q a

subrules1 :: GenRules Subquery1 q
subrules1 SQ1 = pure 610

subrules2 :: HasSubquery1 q => GenRules Subquery2 q
subrules2 SQ2 = fetch $ subquery1 SQ1

data Query a where
  Subquery1 :: Subquery1 a -> Query a
  Subquery2 :: Subquery2 a -> Query a
  Query :: Query Int

instance HasSubquery1 Query where
  subquery1 = Subquery1

instance HasSubquery2 Query where
  subquery2 = Subquery2

rules :: Rules Query
rules (Subquery1 sq) = subrules1 sq
rules (Subquery2 sq) = subrules2 sq
rules Query = pure 611
