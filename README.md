# rock [![Hackage](https://img.shields.io/hackage/v/rock.svg)](https://hackage.haskell.org/package/rock)

A build system inspired by [Build systems à la carte](https://www.microsoft.com/en-us/research/publication/build-systems-la-carte/).

Used in [Sixten](https://github.com/ollef/sixten) and
[Sixty](https://github.com/ollef/sixty) to achieve incremental and query driven
compiler architectures.

# Example

```haskell
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
    result <- Rock.runTask rules (Rock.fetch D)
    print result
  do
    putText "Running with memoisation"
    memoVar <- newMVar mempty
    result <-
      Rock.runTask
        (Rock.memoise memoVar rules)
        (Rock.fetch D)
    print result
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

# Related projects

* [Shake](http://hackage.haskell.org/package/shake)
* [Salsa](https://crates.io/crates/salsa)

# Contributions

... are very welcome, especially in the areas of documentation, examples,
testing, and benchmarking.
