# rock [![Build Status](https://travis-ci.com/ollef/rock.svg?branch=master)](https://travis-ci.com/ollef/rock) [![CI Build Status](https://github.com/ollef/rock/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/ollef/rock/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/rock.svg)](https://hackage.haskell.org/package/rock)



A build system inspired by [Build systems à la carte](https://www.microsoft.com/en-us/research/publication/build-systems-la-carte/).

Used in [Sixten](https://github.com/ollef/sixten),
[Sixty](https://github.com/ollef/sixty) and
[Eclair](https://github.com/luc-tielen/eclair-lang) to achieve incremental and
query driven compiler architectures.

## Example

```haskell
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
```

Prints

```
Running
Fetching D
Fetching B
Fetching A
Fetching C
Fetching A
70
Running with memoisation
Fetching D
Fetching B
Fetching A
Fetching C
70
```

Note: Besides pure computations as shown above, the `Task` data type implements
`MonadIO`, so you can lift IO actions into the `Task` monad by using
`liftIO`!

## Query parameters

If you need to parametrize your queries (e.g. typechecking one specific file),
you can do this by adding additional arguments to your `Query` datatype:

```haskell
data Query a where
  Parse :: FilePath -> Query AST
  Typecheck :: FilePath -> Query (Either TypeError TypedAST)

rules :: Rock.Rules Query
rules key = case key of
  Parse file -> do
    _ -- parse the file..
  Typecheck file -> do
    ast <- Rock.fetch (Parse file)
    _ -- typecheck file..
```

## Related projects

* [Shake](http://hackage.haskell.org/package/shake)
* [Salsa](https://crates.io/crates/salsa)

## Contributions

... are very welcome, especially in the areas of documentation and examples.
