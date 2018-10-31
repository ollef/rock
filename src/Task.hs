{-# language DeriveFunctor #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Task where

import Protolude

import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose

import Hashed
import VerifyingTraces(VT)
import qualified VerifyingTraces as VT

data Env k v = Env
  { tasks :: !(Tasks k v)
  , tasksStartedVar :: !(MVar (DMap k (Compose MVar v)))
  , taskTracesVar :: !(MVar (VT k v))
  }

newtype Task k v a = Task { unTask :: Env k v -> IO a }
  deriving (Functor)

type Tasks k v = forall i. k i -> Task k v (v i)

runTask :: GCompare k => Tasks k v -> VT k v -> Task k v a -> IO (a, VT k v)
runTask ts traces task = do
  tracesVar <- newMVar traces
  startedVar <- newMVar mempty
  a <- unTask task Env
    { tasks = ts
    , tasksStartedVar = startedVar
    , taskTracesVar = tracesVar
    }
  traces' <- readMVar tracesVar
  return (a, traces')

build :: (GCompare k, HashTag k v) => Tasks k v -> VT k v -> k i -> IO (v i, VT k v)
build ts traces key = runTask ts traces $ fetch key

instance Applicative (Task k v) where
  pure = Task . pure . pure
  Task f <*> Task x = Task $ (<*>) <$> f <*> x

instance Monad (Task k v) where
  Task ma >>= f = Task $ \env -> do
    a <- ma env
    unTask (f a) env

instance MonadIO (Task k v) where
  liftIO = Task . const

track :: forall k v a. GCompare k => Task k v a -> Task k v (a, DMap k v)
track (Task task) = Task $ \env -> do
  depsVar <- newMVar mempty
  let
    tasks' :: forall i. k i -> Task k v (v i)
    tasks' k = Task $ \_env -> do
      v <- unTask (tasks env k) env
      modifyMVar_ depsVar $ pure . DMap.insert k v
      return v
  a <- task env { tasks = tasks' }
  deps <- readMVar depsVar
  return (a, deps)

fetchAsync :: (HashTag k v, GCompare k) => k i -> Task k v (Task k v (v i))
fetchAsync key = Task $ \env -> do
  var <- newEmptyMVar
  _ <- forkIO $ do
    value <- unTask (fetch key) env
    putMVar var value
  return $ liftIO $ readMVar var

fetch :: (HashTag k v, GCompare k) => k i -> Task k v (v i)
fetch key = Task $ \env -> do
  putText "fetching"
  let
    fromScratch = do
      putText "fromScratch"
      (value, deps) <- unTask (track $ tasks env key) env
      putText $ "deps " <> show (DMap.size deps)
      modifyMVar_ (taskTracesVar env)
        $ pure
        . VT.record key value deps
      return value

    fetchHash k = hashed k <$> unTask (fetch k) env

    checkDeps = do
      taskTraces <- readMVar $ taskTracesVar env
      case DMap.lookup key taskTraces of
        Nothing -> fromScratch
        Just oldValueDeps -> do
          upToDate <- VT.verifyDependencies (VT.dependencies oldValueDeps) fetchHash
          if upToDate then
            return $ unhashed $ VT.value oldValueDeps
          else
            fromScratch

  join $ modifyMVar (tasksStartedVar env) $ \tasksStarted ->
    case DMap.lookup key tasksStarted of
      Nothing -> do
        valueVar <- newEmptyMVar
        let k = do
              value <- checkDeps
              putMVar valueVar value
              return value
        return (DMap.insert key (Compose valueVar) tasksStarted, k)
      Just (Compose valueVar) ->
        return (tasksStarted, readMVar valueVar)
