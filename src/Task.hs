{-# language DeriveFunctor #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Task where

import Protolude

import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap

import Hashed
import Traces(Traces)
import qualified Traces

-------------------------------------------------------------------------------
-- Types

type Rules f = forall a. f a -> Rule f a

data Rule f a
  = Input (IO a)
  | Task (Task f a)
  deriving Functor

newtype Task f a = MkTask { unTask :: IO (Result f a) }
  deriving Functor

data Result f a
  = Done a
  | Blocked !(BlockedTask f a)
  deriving Functor

data BlockedTask f a where
  BlockedTask :: Block f a -> (a -> Task f b) -> BlockedTask f b

data Block f a where
  Fetch :: f a -> Block f a
  Fork :: !(BlockedTask f (a -> b)) -> !(BlockedTask f a) -> Block f b

-------------------------------------------------------------------------------
-- Instances

instance Applicative (Task f) where
  pure = MkTask . pure . Done
  MkTask mrf <*> MkTask mrx = MkTask $ (<*>) <$> mrf <*> mrx

instance Monad (Task f) where
  (>>) = (*>)
  MkTask ma >>= f = MkTask $ do
    ra <- ma
    case ra of
      Done a -> unTask $ f a
      Blocked (BlockedTask b k) -> return $ Blocked $ BlockedTask b $ k >=> f

instance MonadIO (Task f) where
  liftIO io = MkTask $ pure <$> io

instance Applicative (Result f) where
  pure = Done
  Done f <*> Done x = Done $ f x
  Done f <*> Blocked b = Blocked $ f <$> b
  Blocked b <*> Done x = Blocked $ ($ x) <$> b
  Blocked b1 <*> Blocked b2 = Blocked $ BlockedTask (Fork b1 b2) pure

instance Monad (Result f) where
  (>>) = (*>)
  Done x >>= f = f x
  Blocked (BlockedTask b t) >>= f = Blocked $ BlockedTask b $ t >=> MkTask . pure . f

deriving instance Functor (BlockedTask f)

-------------------------------------------------------------------------------

transFetch
  :: (forall b. f b -> Task f' b)
  -> Task f a
  -> Task f' a
transFetch f task = MkTask $ do
  result <- unTask task
  case result of
    Done a -> return $ Done a
    Blocked b -> unTask $ transFetchBT f b

transFetchBT
  :: (forall b. f b -> Task f' b)
  -> BlockedTask f a
  -> Task f' a
transFetchBT f (BlockedTask b t) = do
  a <- transFetchB f b
  transFetch f $ t a

transFetchB
  :: (forall b. f b -> Task f' b)
  -> Block f a
  -> Task f' a
transFetchB f (Fetch k) = f k
transFetchB f (Fork b1 b2) = transFetchBT f b1 <*> transFetchBT f b2

-------------------------------------------------------------------------------

fetch :: f a -> Task f a
fetch key = MkTask $ pure $ Blocked $ BlockedTask (Fetch key) pure

-------------------------------------------------------------------------------

runTask :: Rules f -> Task f a -> IO a
runTask rules task = do
  result <- unTask task
  case result of
    Done a -> return a
    Blocked b -> runBT rules b

runBT :: Rules f -> BlockedTask f a -> IO a
runBT rules (BlockedTask b f) = do
  a <- runB rules b
  runTask rules $ f a

runB :: Rules f -> Block f a -> IO a
runB rules (Fetch key) = case rules key of
  Input io -> io
  Task t -> runTask rules t
runB rules (Fork bf bx) =
  withAsync (runBT rules bf) $ \af -> do
    x <- runBT rules bx
    f <- wait af
    return $ f x

-------------------------------------------------------------------------------

track :: forall f a. GCompare f => Task f a -> Task f (a, DMap f Identity)
track task = do
  depsVar <- liftIO $ newMVar mempty
  let
    record :: f b -> Task f b
    record key = do
      value <- fetch key
      liftIO $ modifyMVar_ depsVar $ pure . DMap.insert key (Identity value)
      return value
  result <- transFetch record task
  deps <- liftIO $ readMVar depsVar
  return (result, deps)

memoise
  :: forall f
  . GCompare f
  => MVar (DMap f MVar)
  -> Rules f
  -> Rules f
memoise startedVar rules (key :: f a)  =
  case rules key of
    Input io -> Input $ go io
    Task task -> Task $ go task
  where
    go :: MonadIO m => m a -> m a
    go m =
      join $ liftIO $ modifyMVar startedVar $ \started ->
        case DMap.lookup key started of
          Nothing -> do
            valueVar <- newEmptyMVar
            return
              ( DMap.insert key valueVar started
              , do
                value <- m
                liftIO $ putMVar valueVar value
                return value
              )
          Just valueVar ->
            return (started, liftIO $ readMVar valueVar)

verifyTraces
  :: (GCompare f, HashTag f)
  => MVar (Traces f)
  -> Rules f
  -> Rules f
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
    fetchHashed :: HashTag f => f a -> Task f (Hashed a)
    fetchHashed key' = hashed key' <$> fetch key'

build
  :: forall f a
  . (GCompare f, HashTag f)
  => Rules f
  -> Traces f
  -> f a
  -> IO (a, Traces f)
build rules traces key = do
  tracesVar <- newMVar traces
  startedVar <- newMVar mempty
  let
    vtTasks :: Rules f
    vtTasks = memoise startedVar $ verifyTraces tracesVar rules
  value <- runTask vtTasks (fetch key)
  traces' <- readMVar tracesVar
  return (value, traces')
