{- |
Copyright: (c) 2020 John 'Ski
SPDX-License-Identifier: BSD-3-Clause
Maintainer: John 'Ski <riuga@tuta.io>

A resource management monad that prevents usage of dead resources while allowing for fine-tuned reallocation semantics.
-}

module Control.Monad.Critical (
  MonadCritical,
  Critical,
  runCritical,
  InterchangeStrategy,
  performInterchange,
  Preemptive,
  Conservative,
  Cell,
  critically,
  interchange,
  cellularize,
  borrow
) where

import Data.Proxy
import Control.Concurrent.STM
import Control.Monad.Managed

-- * Interchange strategies

class InterchangeStrategy a where
  {-# MINIMAL performInterchange #-}
  performInterchange 
    :: TMVar x 
    -> IO x 
    -> (x -> IO ())
    -> Proxy a
    -> IO ()

-- | An interchange strategy wherein the new resource is allocated beforehand,
-- performing a swap thereafter with existing @Cell@ resource, which in turn gets released.
-- This strategy minimizes thread blocking if the resource does not necessarily need
-- to be released before a new one can be allocated.
data Preemptive

instance InterchangeStrategy Preemptive where
  performInterchange var alloc release _ = do
    newRes <- alloc
    atomically (swapTMVar var newRes) >>= release

-- | An interchange strategy wherein the existing @Cell@ resource is first released
-- before the the allocation takes place and is inserted into the @Cell@. Depending 
-- on how long the allocation action takes to complete, other threads needing to 
-- borrow the @Cell@ may need to block significantly longer compared to the Preemptive
-- strategy.
data Conservative

instance InterchangeStrategy Conservative where
  performInterchange var alloc release _ = do
    atomically (takeTMVar var) >>= release
    newRes <- alloc
    atomically $ putTMVar var newRes

-- * Critical monad

newtype Critical s a
  = Critical { unCritical :: Managed a }
  deriving newtype (Functor, Applicative, Monad, MonadManaged, MonadIO)

class (forall x. MonadIO (m x)) => MonadCritical m where
  critically :: Critical s a -> m s a

instance MonadCritical Critical where
  critically = id

-- ** Execution

-- | Run a critical action. The type parameter @s@ is existentially quantified
-- to prevent usage of released @Cell@s outside the scope of the critical action.
runCritical :: (forall s. Critical s ()) -> IO ()
runCritical = runManaged . unCritical

-- ** Cellular operations

-- | A @Cell@ represents a 'reference' to a resource bound to a particular @MonadCritical@.
-- The @s@ parameter ensures that said resource cannot be leaked out of the scope of said
-- @MonadCritical@. Additionally, the @i@ parameter specifies the @InterchangeStrategy@
-- for a particular @Cell@, which can be restricted at the type level.
--
-- Within a @MonadCritical@, @Cell@s can be @borrow@ed for use in an IO action, or
-- @interchanged@ to be reallocated with semantics dependent upon @i@. @Cell@s, by their
-- very nature, are thread-safe and will block upon borrowing or interchange. This is to
-- prevent the release of a @Cell@ in one thread while it may be concurrently borrowed in
-- another.
data Cell s i a
  = Cell (TMVar a) (a -> IO ())

-- | Create a @Cell@ given an allocator and releaser.
cellularize 
  :: forall i m a s. MonadCritical m
  => IO a 
  -> (a -> IO ()) 
  -> m s (Cell s i a)
cellularize alloc release = do 
  x <- liftIO alloc
  var <- liftIO $ newTMVarIO x
  critically . Critical $ managed (withCell $ Cell var release)
  where
    withCell :: Cell s i a -> (Cell s i a -> IO r) -> IO r
    withCell c@(Cell var _) f = do
      x <- f c
      atomically (takeTMVar var) >>= release
      pure x
      
-- | Given a new allocator, swaps the contents of the @Cell@ and releases the old contents.
-- Exact semantics depend on the @Cell@'s InterchangeStrategy
interchange 
  :: forall i m a s. 
      ( MonadCritical m
      , InterchangeStrategy i
      )
  => Cell s i a 
  -> IO a 
  -> m s ()
interchange (Cell var release) alloc = 
  liftIO $ performInterchange var alloc release (Proxy :: Proxy i)

-- | Given a @Cell@, perform an IO action with it.
borrow :: MonadCritical m => Cell s i a -> (a -> IO b) -> m s b
borrow (Cell v _) f = liftIO $ do
  x <- atomically $ takeTMVar v
  y <- f x
  atomically $ putTMVar v x
  pure y
