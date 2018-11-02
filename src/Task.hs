{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Task where

import Protolude

import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose

import Hashed
import Traces(Traces)
import qualified Traces

data Task k v a where
  Pure :: a -> Task k v a
  Need :: k i -> Task k v a -> Task k v a
  Fetch :: k i -> (v i -> Task k v b) -> Task k v b
  Effect :: IO a -> (a -> Task k v b) -> Task k v b

type Tasks k v = forall i. k i -> Task k v (v i)

mapFetch :: (forall i. k i -> Task k v' (v i)) -> Task k v a -> Task k v' a
mapFetch _ (Pure a) = Pure a
mapFetch f (Need key t) = Need key (mapFetch f t)
mapFetch f (Fetch key k) = do
  value <- f key
  mapFetch f $ k value
mapFetch f (Effect io k) = Effect io (mapFetch f <$> k)

instance Functor (Task k v) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Need key t) = Need key (f <$> t)
  fmap f (Fetch key k) = Fetch key (fmap f <$> k)
  fmap f (Effect io k) = Effect io (fmap f <$> k)

instance Applicative (Task k v) where
  pure = Pure
  Pure f <*> t = f <$> t
  t <*> Pure a = ($ a) <$> t
  Need key t1 <*> t2 = Need key (t1 <*> t2)
  t1 <*> Need key t2 = Need key (t1 <*> t2)
  Effect io k <*> t = Effect io (\a -> k a <*> t)
  t <*> Effect io k = Effect io (\a -> t <*> k a)
  Fetch key1 k1 <*> Fetch key2 k2 = Need key2 $ Fetch key1 (\a -> k1 a <*> Fetch key2 k2)

instance Monad (Task k v) where
  Pure a >>= f = f a
  Need key t1 >>= f = Need key (t1 >>= f)
  Fetch key k >>= f = Fetch key (k >=> f)
  Effect io k >>= f = Effect io (k >=> f)
  (>>) = (*>)

instance MonadIO (Task k v) where
  liftIO io = Effect io pure

runTask :: Tasks k v -> Task k v a -> IO a
runTask tasks task = case task of
  Pure a -> pure a
  Need key t -> do
    _ <- forkIO $ runTask tasks $ void $ fetch key
    runTask tasks t
  Fetch key k -> do
    a <- runTask tasks $ tasks key
    runTask tasks $ k a
  Effect io k -> do
    a <- io
    runTask tasks $ k a

fetch :: k i -> Task k v (v i)
fetch key = Fetch key pure

need :: k i -> Task k v ()
need key = Need key $ pure ()

fetchHashed :: HashTag k v => k i -> Task k v (Hashed v i)
fetchHashed key = hashed key <$> fetch key

track :: forall k v a. GCompare k => Task k v a -> Task k v (a, DMap k v)
track task = do
  depsVar <- liftIO $ newMVar mempty
  let
    go :: k i -> Task k v (v i)
    go key = do
      value <- fetch key
      liftIO $ modifyMVar_ depsVar $ pure . DMap.insert key value
      return value
  result <- mapFetch go task
  deps <- liftIO $ readMVar depsVar
  return (result, deps)

dedup :: GCompare k => MVar (DMap k (Compose MVar v)) -> Tasks k v -> Tasks k v
dedup startedVar tasks key =
  join $ liftIO $ modifyMVar startedVar $ \started ->
    case DMap.lookup key started of
      Nothing -> do
        valueVar <- newEmptyMVar
        let k = do
              value <- tasks key
              liftIO $ putMVar valueVar value
              return value
        return (DMap.insert key (Compose valueVar) started, k)
      Just (Compose valueVar) ->
        return (started, liftIO $ readMVar valueVar)

data Verifier k v t = Verifier
  { verifyKey :: forall i. k i -> t -> Task k v (Maybe (v i))
  , recordValue :: forall i. k i -> v i -> DMap k v -> t -> t
  }

traceVerifier :: (GCompare k, HashTag k v) => Verifier k v (Traces k v)
traceVerifier = Verifier
  { verifyKey = \key traces ->
    case DMap.lookup key traces of
      Nothing -> return Nothing
      Just oldValueDeps ->
        Traces.verifyDependencies fetchHashed oldValueDeps
  , recordValue = Traces.record
  }

verify
  :: GCompare k
  => Verifier k v t
  -> MVar t
  -> Tasks k v
  -> Tasks k v
verify verifier verifierVar tasks key = do
  vt <- liftIO $ readMVar verifierVar
  maybeValue <- verifyKey verifier key vt
  case maybeValue of
    Nothing -> do
      (value, deps) <- track $ tasks key
      liftIO $ modifyMVar_ verifierVar
        $ pure
        . recordValue verifier key value deps
      return value
    Just value -> return value

build
  :: forall k v i
  . (GCompare k, HashTag k v)
  => Tasks k v
  -> Traces k v
  -> k i
  -> IO (v i, Traces k v)
build tasks traces key = do
  tracesVar <- newMVar traces
  startedVar <- newMVar mempty
  let
    vtTasks :: Tasks k v
    vtTasks = verify traceVerifier tracesVar $ dedup startedVar tasks
  value <- runTask vtTasks (fetch key)
  traces' <- readMVar tracesVar
  return (value, traces')
