{-# language DeriveGeneric #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
module Examples where

import Protolude

import Data.Dependent.Sum
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

instance HashTag TaskKey Identity where
  hashTagged ParseModuleHeader {} = hash
  hashTagged ParseModule {} = hash

type CompilerTask = Task TaskKey Identity
type CompilerTasks = Tasks TaskKey Identity

compilerTasks :: CompilerTasks
compilerTasks (ParseModuleHeader mname) = Identity <$> parseModuleHeader mname
compilerTasks (ParseModule mname) = Identity <$> parseModule mname

parseModuleHeader :: ModuleName -> CompilerTask (ModuleHeader, Text)
parseModuleHeader mname = pure (ModuleHeader mname, "")

parseModule :: ModuleName -> CompilerTask ParsedModule
parseModule mname = do
  Identity (header, _t) <- fetch (ParseModuleHeader mname)
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

instance HashTag SheetKey Identity where
  hashTagged A = hash
  hashTagged B = hash
  hashTagged C = hash
  hashTagged D = hash

instance GShow SheetKey where
  gshowsPrec = showsPrec

instance ShowTag SheetKey Identity where
  showTaggedPrec A = showsPrec
  showTaggedPrec B = showsPrec
  showTaggedPrec C = showsPrec
  showTaggedPrec D = showsPrec

type SheetTask = Task SheetKey Identity
type SheetTasks = Tasks SheetKey Identity

sheetTasks :: SheetTasks
sheetTasks key = do
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

sheetTasks2 :: SheetTasks
sheetTasks2 key = do
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
