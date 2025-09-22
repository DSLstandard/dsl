module DSL.Namespace where

import Data.HashMap.Strict qualified as HashMap
import Relude

newtype Namespace a = Namespace
  { unNamespace :: IORef (HashMap Text a)
  }

create :: (MonadIO m) => m (Namespace a)
create = liftIO do
  Namespace <$> newIORef HashMap.empty

fromEntries :: (MonadIO m) => [(Text, a)] -> m (Namespace a)
fromEntries entries = do
  Namespace <$> newIORef (HashMap.fromList entries)

-- | Union but the RHS dominates.
include :: Namespace a -> Namespace a -> IO ()
include (Namespace self) (Namespace rhs) = do
  rhs <- readIORef rhs
  modifyIORef' self (<> rhs)

toEntries :: (MonadIO m) => Namespace a -> m [(Text, a)]
toEntries (Namespace m) = do
  m <- readIORef m
  pure $ HashMap.toList m

insert :: (MonadIO m) => Namespace a -> Text -> a -> m ()
insert (Namespace m) key value = do
  modifyIORef' m (HashMap.insert key value)

lookup :: (MonadIO m) => Namespace a -> Text -> m (Maybe a)
lookup (Namespace m) key = do
  m <- readIORef m
  pure $ HashMap.lookup key m
