{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
module Examples where

import Protolude

import Data.Dependent.Sum
import Data.Functor.Classes
import Data.GADT.Compare
import Data.GADT.Show
import Text.Show

import Hashed
import Task

-------------------------------------------------------------------------------
data ModuleName = ModuleName
  deriving (Eq, Ord, Show, Generic)
instance Hashable ModuleName
data ModuleHeader = ModuleHeader ModuleName
  deriving (Eq, Ord, Show, Generic)
instance Hashable ModuleHeader
data ParsedModule = ParsedModule ModuleHeader
  deriving (Eq, Ord, Show, Generic)
instance Hashable ParsedModule

data TaskKey a where
  ParseModuleHeader :: ModuleName -> TaskKey (ModuleHeader, Text)
  ParseModule :: ModuleName -> TaskKey ParsedModule

instance GEq TaskKey where
  geq a b = case gcompare a b of
    GLT -> Nothing
    GEQ -> Just Refl
    GGT -> Nothing

gcompareEq :: Ord x => x -> x -> GOrdering a a
gcompareEq x y = case compare x y of
  EQ -> GEQ
  LT -> GLT
  GT -> GGT

instance GCompare TaskKey where
  gcompare (ParseModuleHeader x) (ParseModuleHeader y) = gcompareEq x y
  gcompare (ParseModule x) (ParseModule y) = gcompareEq x y
  gcompare ParseModuleHeader {} _ = GLT
  gcompare _ ParseModuleHeader {} = GGT

deriving instance Show (TaskKey a)

instance HashTag TaskKey where
  hashTagged ParseModuleHeader {} = hash
  hashTagged ParseModule {} = hash

type CompilerTask = Task TaskKey
type CompilerRules = Rules TaskKey

compilerRules :: CompilerRules
compilerRules (ParseModuleHeader mname) = Task $ parseModuleHeader mname
compilerRules (ParseModule mname) = Task $ parseModule mname

parseModuleHeader :: ModuleName -> CompilerTask (ModuleHeader, Text)
parseModuleHeader mname = pure (ModuleHeader mname, "")

parseModule :: ModuleName -> CompilerTask ParsedModule
parseModule mname = do
  (header, _t) <- fetch (ParseModuleHeader mname)
  pure $ ParsedModule header

-------------------------------------------------------------------------------
data SheetKey a where
  A :: SheetKey Integer
  B :: SheetKey Integer
  C :: SheetKey Integer
  D :: SheetKey Integer

instance GEq SheetKey where
  geq a b = case gcompare a b of
    GLT -> Nothing
    GEQ -> Just Refl
    GGT -> Nothing

instance GCompare SheetKey where
  gcompare A A = GEQ
  gcompare B B = GEQ
  gcompare C C = GEQ
  gcompare D D = GEQ
  gcompare A _ = GLT
  gcompare _ A = GGT
  gcompare B _ = GLT
  gcompare _ B = GGT
  gcompare C _ = GLT
  gcompare _ C = GGT

deriving instance Show (SheetKey a)

instance HashTag SheetKey where
  hashTagged A = hash
  hashTagged B = hash
  hashTagged C = hash
  hashTagged D = hash

instance GShow SheetKey where
  gshowsPrec = showsPrec

instance Show1 g => ShowTag SheetKey g where
  showTaggedPrec A = showsPrec1
  showTaggedPrec B = showsPrec1
  showTaggedPrec C = showsPrec1
  showTaggedPrec D = showsPrec1

type SheetTask = Task SheetKey
type SheetRules = Rules SheetKey

sheetRules :: SheetRules
sheetRules key = Task $ do
  liftIO $ putText $ "computing " <> Protolude.show key
  case key of
    A -> pure 10
    B -> do
      a <- fetch A
      pure $ a + 20
    C -> do
      a <- fetch A
      pure $ a + 30
    D -> do
      b <- fetch B
      c <- fetch C
      pure $ b + c

sheetRules2 :: SheetRules
sheetRules2 key = Task $ do
  liftIO $ putText $ "computing 2 " <> Protolude.show key
  case key of
    A -> pure 12
    B -> do
      a <- fetch A
      pure $ a + 10
    C -> do
      a <- fetch A
      pure $ a + 20
    D -> (+) <$> fetch B <*> fetch C
