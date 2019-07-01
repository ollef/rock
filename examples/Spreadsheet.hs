{-# language GADTs #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}

import Protolude

import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import qualified Rock

data Query a where
  A :: Query Integer
  B :: Query Integer
  C :: Query Integer
  D :: Query Integer

deriving instance Show (Query a)

deriveGEq ''Query
deriveGCompare ''Query

rules :: Rock.Rules Query
rules key = do
  putText $ "Fetching " <> show key
  case key of
    A -> pure 10
    B -> do
      a <- Rock.fetch A
      pure $ a + 20
    C -> do
      a <- Rock.fetch A
      pure $ a + 30
    D ->
      (+) <$> Rock.fetch B <*> Rock.fetch C

main :: IO ()
main = do
  do
    putText "Running"
    result <- Rock.runTask Rock.sequentially rules (Rock.fetch D)
    print result
  do
    putText "Running with memoisation"
    memoVar <- newMVar mempty
    result <-
      Rock.runTask
        Rock.sequentially
        (Rock.memoise memoVar rules)
        (Rock.fetch D)
    print result
  do
    putText "Running with memoisation using the parallel strategy"
    memoVar <- newMVar mempty
    result <-
      Rock.runTask
        Rock.inParallel
        (Rock.memoise memoVar rules)
        (Rock.fetch D)
    print result
