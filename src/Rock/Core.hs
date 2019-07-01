{-# language CPP #-}
{-# language DefaultSignatures #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
module Rock.Core where

#if MIN_VERSION_base(4,12,0)
import Protolude hiding (Ap)
#else
import Protolude
#endif

import Control.Monad.Cont
import Control.Monad.Identity
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Data.Dependent.Map(DMap, GCompare)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare

import Rock.Hashed
import Rock.HashTag
import Rock.Traces(Traces)
import qualified Rock.Traces as Traces

-------------------------------------------------------------------------------
-- * Types

-- | A function which, given an @f@ query, returns a 'Task' allowed to make @f@
-- queries to compute its result.
type Rules f = GenRules f f

-- | A function which, given an @f@ query, returns a 'Task' allowed to make @g@
-- queries to compute its result.
type GenRules f g = forall a. f a -> Task g a

-- | An @IO@ action that is allowed to make @f@ queries using the 'fetch'
-- method from its 'MonadFetch' instance.
newtype Task f a = Task { unTask :: IO (Result f a) }

-- | The result of a @Task@, which is either done or wanting to make one or
-- more @f@ queries.
data Result f a
  = Done a
  | Blocked !(BlockedTask f a)

data BlockedTask f a where
  BlockedTask :: Block f a -> (a -> Task f b) -> BlockedTask f b

data Block f a where
  Fetch :: f a -> Block f a
  Ap :: !(BlockedTask f (a -> b)) -> !(BlockedTask f a) -> Block f b

-------------------------------------------------------------------------------
-- * Fetch class

-- | Monads that can make @f@ queries by 'fetch'ing them.
class Monad m => MonadFetch f m | m -> f where
  fetch :: f a -> m a
  default fetch
    :: (MonadTrans t, MonadFetch f m1, m ~ t m1)
    => f a
    -> m a
  fetch = lift . fetch

instance MonadFetch f m => MonadFetch f (ContT r m)
instance MonadFetch f m => MonadFetch f (ExceptT e m)
instance MonadFetch f m => MonadFetch f (IdentityT m)
instance MonadFetch f m => MonadFetch f (MaybeT m)
instance MonadFetch f m => MonadFetch f (ReaderT r m)
instance (MonadFetch f m, Monoid w) => MonadFetch f (Strict.RWST r w s m)
instance (MonadFetch f m, Monoid w) => MonadFetch f (Lazy.RWST r w s m)
instance MonadFetch f m => MonadFetch f (Strict.StateT s m)
instance MonadFetch f m => MonadFetch f (Lazy.StateT s m)
instance (Monoid w, MonadFetch f m) => MonadFetch f (Strict.WriterT w m)
instance (Monoid w, MonadFetch f m) => MonadFetch f (Lazy.WriterT w m)

-------------------------------------------------------------------------------
-- Instances

instance Functor (Task f) where
  {-# INLINE fmap #-}
  fmap f (Task t) = Task $ fmap f <$> t

-- Note: This instance might not fully evaluate @t1@ before @t2@ in
-- @t1 '<*>' t2@ in case @t1@ performs a query using 'fetch'. If this
-- is not desirable, use 'Sequential'.
instance Applicative (Task f) where
  {-# INLINE pure #-}
  pure = Task . pure . Done
  {-# INLINE (<*>) #-}
  Task mrf <*> Task mrx = Task $ (<*>) <$> mrf <*> mrx

instance Monad (Task f) where
  {-# INLINE (>>) #-}
  (>>) = (*>)
  {-# INLINE (>>=) #-}
  Task ma >>= f = Task $ do
    ra <- ma
    case ra of
      Done a -> unTask $ f a
      Blocked (BlockedTask b k) -> return $ Blocked $ BlockedTask b $ k >=> f

instance MonadIO (Task f) where
  {-# INLINE liftIO #-}
  liftIO io = Task $ pure <$> io

instance MonadFetch f (Task f) where
  fetch key = Task $ pure $ Blocked $ BlockedTask (Fetch key) pure

instance Functor (Result f) where
  {-# INLINE fmap #-}
  fmap f (Done x) = Done $ f x
  fmap f (Blocked b) = Blocked $ f <$> b

instance Applicative (Result f) where
  {-# INLINE pure #-}
  pure = Done
  {-# INLINE (<*>) #-}
  Done f <*> Done x = Done $ f x
  Done f <*> Blocked b = Blocked $ f <$> b
  Blocked b <*> Done x = Blocked $ ($ x) <$> b
  Blocked b1 <*> Blocked b2 = Blocked $ BlockedTask (Ap b1 b2) pure

instance Monad (Result f) where
  {-# INLINE (>>) #-}
  (>>) = (*>)
  {-# INLINE (>>=) #-}
  Done x >>= f = f x
  Blocked (BlockedTask b t) >>= f = Blocked $ BlockedTask b $ t >=> Task . pure . f

instance Functor (BlockedTask f) where
  {-# INLINE fmap #-}
  fmap f (BlockedTask b t) = BlockedTask b $ fmap f <$> t

-------------------------------------------------------------------------------
-- * Transformations

-- | Transform the type of queries that a 'Task' performs.
transFetch
  :: (forall b. f b -> Task f' b)
  -> Task f a
  -> Task f' a
transFetch f task = Task $ do
  result <- unTask task
  case result of
    Done a -> return $ Done a
    Blocked b -> unTask $ transFetchBlockedTask f b

transFetchBlockedTask
  :: (forall b. f b -> Task f' b)
  -> BlockedTask f a
  -> Task f' a
transFetchBlockedTask f (BlockedTask b t) = do
  a <- transFetchBlock f b
  transFetch f $ t a

transFetchBlock
  :: (forall b. f b -> Task f' b)
  -> Block f a
  -> Task f' a
transFetchBlock f (Fetch k) = f k
transFetchBlock f (Ap b1 b2) = transFetchBlockedTask f b1 <*> transFetchBlockedTask f b2

-------------------------------------------------------------------------------
-- * Strategies

-- | A 'Strategy' specifies how two queries are performed in an 'Applicative'
-- context.
type Strategy = forall a b. IO (a -> b) -> IO a -> IO b

-- | Runs the two queries in sequence.
sequentially :: Strategy
sequentially = (<*>)

-- | Runs the two queries in parallel.
inParallel :: Strategy
inParallel mf mx = withAsync mf $ \af -> do
  x <- mx
  f <- wait af
  return $ f x

-- | Uses the underlying instances, except for the Applicative instance which
-- is defined in terms of 'return' and '(>>=)'.
--
-- When used with 'Task', i.e. if you construct @m :: 'Sequential' ('Task' f)
-- a@, this means that fetches within @m@ are done sequentially.
newtype Sequential m a = Sequential { runSequential :: m a }
  deriving (Functor, Monad, MonadIO, MonadFetch f)

-- | Defined in terms of 'return' and '(>>=)'.
instance Monad m => Applicative (Sequential m) where
  {-# INLINE pure #-}
  pure = Sequential . return
  {-# INLINE (<*>) #-}
  Sequential mf <*> Sequential mx = Sequential $ mf >>= \f -> fmap f mx

-------------------------------------------------------------------------------
-- * Running tasks

-- | Perform a 'Task', fetching dependency queries from the given 'Rules' function and using the given 'Strategy' for fetches in an 'Applicative' context.
runTask :: Strategy -> Rules f -> Task f a -> IO a
runTask strategy rules task = do
  result <- unTask task
  case result of
    Done a -> return a
    Blocked b -> runBlockedTask strategy rules b

runBlockedTask :: Strategy -> Rules f -> BlockedTask f a -> IO a
runBlockedTask strategy rules (BlockedTask b f) = do
  a <- runBlock strategy rules b
  runTask strategy rules $ f a

runBlock :: Strategy -> Rules f -> Block f a -> IO a
runBlock strategy rules (Fetch key) =
  runTask strategy rules $ rules key
runBlock strategy rules (Ap bf bx) =
  strategy (runBlockedTask strategy rules bf) (runBlockedTask strategy rules bx)

-------------------------------------------------------------------------------
-- * Task combinators

-- | Track the query dependencies of a 'Task' in a 'DMap'
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

-- | Remember what @f@ queries have already been performed and their results in
-- a 'DMap', and reuse them if a query is performed again a second time.
--
-- The 'DMap' should typically not be reused if there has been some change that
-- might make a query return a different result.
memoise
  :: forall f g
  . GCompare f
  => MVar (DMap f MVar)
  -> GenRules f g
  -> GenRules f g
memoise startedVar rules (key :: f a) =
  join $ liftIO $ modifyMVar startedVar $ \started ->
    case DMap.lookup key started of
      Nothing -> do
        valueVar <- newEmptyMVar
        return
          ( DMap.insert key valueVar started
          , do
            value <- rules key
            liftIO $ putMVar valueVar value
            return value
          )
      Just valueVar ->
        return (started, liftIO $ readMVar valueVar)

-- | Remember the results of previous @f@ queries and what their dependencies
-- were then.
--
-- If all dependencies of a 'NonInput' query are the same, reuse the old result.
-- The 'DMap' _can_ be reused if there are changes to 'Input' queries.
verifyTraces
  :: (GCompare f, HashTag f)
  => MVar (Traces f)
  -> GenRules (Writer TaskKind f) f
  -> Rules f
verifyTraces tracesVar rules key = do
  traces <- liftIO $ readMVar tracesVar
  maybeValue <- case DMap.lookup key traces of
    Nothing -> return Nothing
    Just oldValueDeps ->
      Traces.verifyDependencies fetchHashed oldValueDeps
  case maybeValue of
    Nothing -> do
      ((value, taskKind), deps) <- track $ rules $ Writer key
      case taskKind of
        Input ->
          return ()
        NonInput ->
          liftIO $ modifyMVar_ tracesVar
            $ pure
            . Traces.record key value deps
      return value
    Just value -> return value
  where
    fetchHashed :: HashTag f => f a -> Task f (Hashed a)
    fetchHashed key' = hashed key' <$> fetch key'

data TaskKind
  = Input -- ^ Used for tasks whose results can change independently of their fetched dependencies, i.e. inputs.
  | NonInput -- ^ Used for task whose results only depend on fetched dependencies.

-- | A query that returns a @w@ alongside the ordinary @a@.
data Writer w f a where
  Writer :: f a -> Writer w f (a, w)

instance GEq f => GEq (Writer w f) where
  geq (Writer f) (Writer g) = case geq f g of
    Nothing -> Nothing
    Just Refl -> Just Refl

instance GCompare f => GCompare (Writer w f) where
  gcompare (Writer f) (Writer g) = case gcompare f g of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT

-- | @'writer' write rules@ runs @write w@ each time a @w@ is returned from a
-- rule in @rules@.
writer
  :: forall f w g
  . (forall a. f a -> w -> Task g ())
  -> GenRules (Writer w f) g
  -> GenRules f g
writer write rules key = do
  (res, w) <- rules $ Writer key
  write key w
  return res

-- | @'traceFetch' before after rules@ runs @before q@ before a query is
-- performed from @rules@, and @after q result@ every time a query returns with
-- result @result@. 
traceFetch
  :: (forall a. f a -> Task g ())
  -> (forall a. f a -> a -> Task g ())
  -> GenRules f g
  -> GenRules f g
traceFetch before after rules key = do
  before key
  result <- rules key
  after key result
  return result
