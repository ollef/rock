{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}

import Control.Monad.IO.Class
import Data.GADT.Compare.TH (deriveGEq)
import Data.Hashable
import Data.Some
import Data.IORef
import qualified Rock

data Query a where
  A :: Query Integer
  B :: Query Integer
  C :: Query Integer
  D :: Query Integer

deriving instance Show (Query a)
deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt query =
    case query of
      A -> hashWithSalt salt (0 :: Int)
      B -> hashWithSalt salt (1 :: Int)
      C -> hashWithSalt salt (2 :: Int)
      D -> hashWithSalt salt (3 :: Int)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) = hashWithSalt salt query

rules :: Rock.Rules Query
rules key = do
  liftIO $ putStrLn $ "Fetching " <> show key
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
    liftIO $ putStrLn "Running"
    result <- Rock.runTask rules (Rock.fetch D)
    print result
  do
    liftIO $ putStrLn "Running with memoisation"
    memoVar <- newIORef mempty
    result <- Rock.runTask (Rock.memoise memoVar rules) (Rock.fetch D)
    liftIO $ print result
