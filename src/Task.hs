{-# language DeriveFunctor #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Task where

import Protolude

import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose

import Hashed
import Traces(Traces)
import qualified Traces

-------------------------------------------------------------------------------
-- Types

type Rules k v = forall i. k i -> Rule k v (v i)

data Rule k v a
  = Input (IO a)
  | Task (Task k v a)
  deriving Functor

newtype Task k v a = MkTask { unTask :: IO (Result k v a) }
  deriving Functor

data Result k v a
  = Done a
  | Blocked !(BlockedTask k v a)
  deriving Functor

data BlockedTask k v a where
  BlockedTask :: Block k v a -> (a -> Task k v b) -> BlockedTask k v b

data Block k v a where
  Fetch :: k i -> Block k v (v i)
  Fork :: !(BlockedTask k v (a -> b)) -> !(BlockedTask k v a) -> Block k v b

-------------------------------------------------------------------------------
-- Instances

instance Applicative (Task k v) where
  pure = MkTask . pure . Done
  MkTask mrf <*> MkTask mrx = MkTask $ (<*>) <$> mrf <*> mrx

instance Monad (Task k v) where
  (>>) = (*>)
  MkTask ma >>= f = MkTask $ do
    ra <- ma
    case ra of
      Done a -> unTask $ f a
      Blocked (BlockedTask b k) -> return $ Blocked $ BlockedTask b $ k >=> f

instance MonadIO (Task k v) where
  liftIO io = MkTask $ pure <$> io

instance Applicative (Result k v) where
  pure = Done
  Done f <*> Done x = Done $ f x
  Done f <*> Blocked b = Blocked $ f <$> b
  Blocked b <*> Done x = Blocked $ ($ x) <$> b
  Blocked b1 <*> Blocked b2 = Blocked $ BlockedTask (Fork b1 b2) pure

instance Monad (Result k v) where
  (>>) = (*>)
  Done x >>= f = f x
  Blocked (BlockedTask b t) >>= f = Blocked $ BlockedTask b $ t >=> MkTask . pure . f

deriving instance Functor (BlockedTask k v)

-------------------------------------------------------------------------------

afterFetch
  :: (forall i. k i -> v' i -> Task k v' (v i))
  -> Task k v a
  -> Task k v' a
afterFetch f task = MkTask $ do
  result <- unTask task
  case result of
    Done a -> return $ Done a
    Blocked b -> return $ Blocked $ afterFetchBT f b

afterFetchBT
  :: (forall i. k i -> v' i -> Task k v' (v i))
  -> BlockedTask k v a
  -> BlockedTask k v' a
afterFetchBT f (BlockedTask b t) = case afterFetchB f b of
  BlockedTask b' t' -> BlockedTask b' $ t' >=> afterFetch f <$> t

afterFetchB
  :: (forall i. k i -> v' i -> Task k v' (v i))
  -> Block k v a
  -> BlockedTask k v' a
afterFetchB f (Fetch k) = BlockedTask (Fetch k) (f k)
afterFetchB f (Fork b1 b2) = BlockedTask (Fork (afterFetchBT f b1) (afterFetchBT f b2)) pure

-------------------------------------------------------------------------------

fetch :: k i -> Task k v (v i)
fetch key = MkTask $ pure $ Blocked $ BlockedTask (Fetch key) pure

-------------------------------------------------------------------------------

runTask :: Rules k v -> Task k v a -> IO a
runTask rules task = do
  result <- unTask task
  case result of
    Done a -> return a
    Blocked b -> runBT rules b

runBT :: Rules k v -> BlockedTask k v a -> IO a
runBT rules (BlockedTask b f) = do
  a <- runB rules b
  runTask rules $ f a

runB :: Rules k v -> Block k v a -> IO a
runB rules (Fetch key) = case rules key of
  Input io -> io
  Task t -> runTask rules t
runB rules (Fork bf bx) =
  withAsync (runBT rules bf) $ \af -> do
    x <- runBT rules bx
    f <- wait af
    return $ f x

-------------------------------------------------------------------------------

track :: forall k v a. GCompare k => Task k v a -> Task k v (a, DMap k v)
track task = do
  depsVar <- liftIO $ newMVar mempty
  let
    record :: k i -> v i -> Task k v (v i)
    record key value = do
      liftIO $ modifyMVar_ depsVar $ pure . DMap.insert key value
      return value
  result <- afterFetch record task
  deps <- liftIO $ readMVar depsVar
  return (result, deps)

memoise
  :: forall k v
  . GCompare k
  => MVar (DMap k (Compose MVar v))
  -> Rules k v
  -> Rules k v
memoise startedVar rules (key :: k i)  =
  case rules key of
    Input io -> Input $ go io
    Task task -> Task $ go task
  where
    go :: MonadIO m => m (v i) -> m (v i)
    go m =
      join $ liftIO $ modifyMVar startedVar $ \started ->
        case DMap.lookup key started of
          Nothing -> do
            valueVar <- newEmptyMVar
            return
              ( DMap.insert key (Compose valueVar) started
              , do
                value <- m
                liftIO $ putMVar valueVar value
                return value
              )
          Just (Compose valueVar) ->
            return (started, liftIO $ readMVar valueVar)

verifyTraces
  :: (GCompare k, HashTag k v)
  => MVar (Traces k v)
  -> Rules k v
  -> Rules k v
verifyTraces tracesVar rules key = case rules key of
  Input io -> Input io
  Task task -> Task $ do
    traces <- liftIO $ readMVar tracesVar
    maybeValue <- case DMap.lookup key traces of
      Nothing -> return Nothing
      Just oldValueDeps ->
        Traces.verifyDependencies fetchHashed oldValueDeps
    case maybeValue of
      Nothing -> do
        (value, deps) <- track task
        liftIO $ modifyMVar_ tracesVar
          $ pure
          . Traces.record key value deps
        return value
      Just value -> return value
  where
    fetchHashed :: HashTag k v => k i -> Task k v (Hashed v i)
    fetchHashed key' = hashed key' <$> fetch key'

build
  :: forall k v i
  . (GCompare k, HashTag k v)
  => Rules k v
  -> Traces k v
  -> k i
  -> IO (v i, Traces k v)
build rules traces key = do
  tracesVar <- newMVar traces
  startedVar <- newMVar mempty
  let
    vtTasks :: Rules k v
    vtTasks = memoise startedVar $ verifyTraces tracesVar rules
  value <- runTask vtTasks (fetch key)
  traces' <- readMVar tracesVar
  return (value, traces')
