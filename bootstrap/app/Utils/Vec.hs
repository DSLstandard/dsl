module Utils.Vec where

import Data.Foldable qualified as Foldable
import Data.Vector qualified as V
import Relude

-- NOTE: This is awful but good enough at the moment.
newtype Vec a = Vec
  { internal :: IORef (V.Vector a)
  }

new :: forall a m. (MonadIO m) => m (Vec a)
new = do
  ref <- liftIO $ newIORef V.empty
  pure $ Vec ref

length :: (MonadIO m) => Vec a -> m Int
length (Vec ref) = do
  v <- liftIO $ readIORef ref
  pure $ V.length v

-- | Check whether the vector is empty.
null :: (MonadIO m) => Vec a -> m Bool
null (Vec ref) = do
  v <- liftIO $ readIORef ref
  pure $ V.null v

at :: (MonadIO m) => Vec a -> Int -> m a
at (Vec ref) i = do
  v <- liftIO $ readIORef ref
  pure $ v V.! i

set :: (MonadIO m) => Vec a -> Int -> a -> m ()
set (Vec ref) i a = do
  v <- liftIO $ readIORef ref
  let v' = v V.// [(i, a)]
  liftIO $ writeIORef ref v'

clear :: (MonadIO m) => Vec a -> m ()
clear (Vec ref) = do
  liftIO $ writeIORef ref V.empty

append :: (MonadIO m) => Vec a -> a -> m ()
append (Vec ref) a = do
  v <- liftIO $ readIORef ref
  let v' = V.snoc v a
  liftIO $ writeIORef ref v'

prepend :: (MonadIO m) => Vec a -> a -> m ()
prepend (Vec ref) a = do
  v <- liftIO $ readIORef ref
  let v' = V.cons a v
  liftIO $ writeIORef ref v'

extendFromVec :: (MonadIO m) => Vec a -> Vec a -> m ()
extendFromVec (Vec self) (Vec other) = do
  other <- liftIO $ readIORef other
  modifyIORef' self (<> other)

extend :: (MonadIO m, Foldable f) => Vec a -> f a -> m ()
extend (Vec self) items = do
  let other = V.fromList (Foldable.toList items)
  modifyIORef' self (<> other)

toList :: (MonadIO m) => Vec a -> m [a]
toList (Vec ref) = do
  v <- liftIO $ readIORef ref
  pure $ V.toList v
