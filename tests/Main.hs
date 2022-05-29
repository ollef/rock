{-# language CPP #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
module Main where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import qualified Data.Dependent.HashMap as DHashMap
import Data.Functor.Const
import Data.GADT.Compare
import Data.GADT.Show
import Data.Hashable
import Data.IORef
import Data.List (sort)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Some
import Data.Type.Equality ((:~:)(Refl))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Rock

data Key v where
  IntKey :: Int -> Key Int
  StringKey :: String -> Key String

deriving instance Show (Key v)

instance GShow Key where
  gshowsPrec = showsPrec

instance GShow (Writer w Key) where
  gshowsPrec d (Writer key) = showsPrec d key

instance Hashable (Some Key) where
  hashWithSalt salt (Some key) =
    case key of
      IntKey i -> hashWithSalt salt (0 :: Int, i)
      StringKey s -> hashWithSalt salt (1 :: Int, s)

instance GEq Key where
  geq (IntKey i1) (IntKey i2)
    | i1 == i2 =
      Just Refl
  geq (StringKey s1) (StringKey s2)
    | s1 == s2 =
      Just Refl
  geq _ _ =
    Nothing

instance GCompare Key where
  gcompare (IntKey i1) (IntKey i2)
    | i1 == i2 =
      GEQ
    | i1 < i2 =
      GLT
    | otherwise =
      GGT
  gcompare (IntKey _) _ =
    GLT
  gcompare _ (IntKey _) =
    GGT
  gcompare (StringKey s1) (StringKey s2)
    | s1 == s2 =
      GEQ
    | s1 < s2 =
      GLT
    | otherwise =
      GGT

deriveArgDict ''Key

int :: Gen Int
int = Gen.int (Range.linear 0 100)

string :: Gen String
string = Gen.string (Range.linear 0 100) Gen.ascii

key :: Gen (Some Key)
key =
  Gen.choice
    [ Some . IntKey <$> int
    , Some . StringKey <$> string
    ]

addRules :: Rules Key
addRules key_ =
  case key_ of
    IntKey i ->
      pure $ i + 1

    StringKey s ->
      pure $ s <> "a"

withKeyFetchedCallback :: (Some f -> IO ()) -> GenRules f g -> GenRules f g
withKeyFetchedCallback keyFetched rules key_ = do
  liftIO $ keyFetched $ Some key_
  rules key_

prop_track_tracks :: Property
prop_track_tracks =
  property $ do
    Some key_ <- forAll key
    startedVar <- liftIO $ newIORef mempty
    let
      rules :: Rules Key
      rules =
        memoise startedVar addRules

    ((), deps) <- liftIO $ runTask rules $ do
      void $ fetch key_
      track (\_ _ -> Const ()) $ void $ fetch key_

    DHashMap.keys deps === [Some key_]

prop_memoise_memoises :: Property
prop_memoise_memoises =
  property $ do
    Some key_ <- forAll key
    fetchedKeysVar <- liftIO $ newIORef []
    startedVar <- liftIO $ newIORef mempty
    let
      keyFetched k =
        atomicModifyIORef fetchedKeysVar $ \ks -> (k : ks, ())

      rules :: Rules Key
      rules =
        memoise startedVar (withKeyFetchedCallback keyFetched addRules)

    liftIO $ runTask rules $ do
      void $ fetch key_
      void $ fetch key_

    fetchedKeys <- liftIO $ readIORef fetchedKeysVar
    fetchedKeys === [Some key_]

inputRules :: Int -> GenRules (Writer TaskKind Key) Key
inputRules input (Writer key_) =
  case key_ of
    IntKey 0 -> do
      pure (input, Input)

    IntKey i -> do
      pure (i + 1, NonInput)

    StringKey "dependent" -> do
      i <- fetch $ IntKey 0
      j <- fetch $ IntKey 1
      pure (show i <> show j, NonInput)

    StringKey s -> do
      i <- fetch $ IntKey 1
      j <- fetch $ IntKey 2
      pure (s <> show i <> show j, NonInput)

prop_verifyTraces :: Property
prop_verifyTraces =
  property $ do
    fetchedKeysVar <- liftIO $ newIORef []
    startedVar <- liftIO $ newIORef mempty
    tracesVar <- liftIO $ newIORef mempty
    let
      keyFetched k =
        atomicModifyIORef fetchedKeysVar $ \ks -> (k : ks, ())

      rules :: Int -> Rules Key
      rules input =
        memoise startedVar $
        verifyTraces
          tracesVar
          (\query value ->
            pure $ Const $ has' @Hashable @Identity query $ hash $ Identity value
          ) $
        withKeyFetchedCallback keyFetched $
        inputRules input

    nonDependentKey <- forAll $ Gen.filter (/= "dependent") string

    liftIO $ runTask (rules 1) $ do
      void $ fetch $ StringKey "dependent"
      void $ fetch $ StringKey nonDependentKey

    liftIO $ atomicWriteIORef startedVar mempty
    liftIO $ atomicWriteIORef fetchedKeysVar mempty

    liftIO $ runTask (rules 2) $ do
      void $ fetch $ StringKey "dependent"
      void $ fetch $ StringKey nonDependentKey

    fetchedKeys <- liftIO $ readIORef fetchedKeysVar
    sort fetchedKeys === [Some $ Writer $ IntKey 0, Some $ Writer $ StringKey "dependent"]

main :: IO ()
main =
  void $ checkParallel $$(discover)
