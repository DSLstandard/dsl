module Utils.Misc where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Traversable
import Data.Vector qualified as V
import Relude

panic :: (HasCallStack) => Text -> a
panic msg = error $ "panicked: " <> msg

impossible :: (HasCallStack) => a
impossible = panic "reached impossible"

{-# DEPRECATED todo "Found TODO code" #-}
todo :: (HasCallStack) => Text -> a
todo msg = panic $ "reached todo: " <> msg

-- | @applyModifiers [f1, f2, f3] x = f3 (f2 (f1 x))@
applyModifiers :: [a -> a] -> a -> a
applyModifiers fs x = foldl' (\x f -> f x) x fs

-- | Safe version of 'last' that returns 'Nothing' on an empty list.
lastMaybe :: [a] -> Maybe a
lastMaybe list = last <$> nonEmpty list

{- | Example:

>>> splits "abc"
[("", "abc"), ("a", "bc"), ("ab", "c"), ("abc", "")]
-}
splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

monoidMapLookup :: (Monoid m, Hashable k) => k -> HashMap k m -> m
monoidMapLookup key map = fromMaybe mempty (HashMap.lookup key map)

monoidMapUpdate :: (Monoid m, Eq m, Hashable k) => k -> (m -> m) -> HashMap k m -> HashMap k m
monoidMapUpdate key mod map = do
  let oldVal = monoidMapLookup key map
  let newVal = mod oldVal
  if newVal == mempty
    then HashMap.delete key map
    else HashMap.insert key newVal map

getElem :: (a -> Bool) -> [a] -> Maybe (Int, a)
getElem p = go 0
 where
  go _ [] = Nothing
  go i (y : ys)
    | p y = Just (i, y)
    | otherwise = go (i + 1) ys

elemIndex' :: (Eq k) => k -> [(k, v)] -> Maybe (Int, v)
elemIndex' _key [] = Nothing
elemIndex' key ((k, v) : kvs)
  | key == k = Just (0, v)
  | otherwise = do
      (i, v) <- elemIndex' key kvs
      pure (i + 1, v)

findMapped :: (Foldable f) => (a -> Maybe b) -> f a -> Maybe b
findMapped f = go . toList
 where
  go [] = Nothing
  go (x : xs) =
    case f x of
      Just y -> Just y
      Nothing -> go xs

{- | Like zip, but keeps all elements from the left list, filling in Nothing for
missing elements on the right.
-}
zipStrongLeft :: [a] -> [b] -> [(a, Maybe b)]
zipStrongLeft as bs = go as bs
 where
  go [] _ = []
  go (a : as) [] = (a, Nothing) : go as []
  go (a : as) (b : bs) = (a, Just b) : go as bs

asumOr :: (Foldable f) => f (Maybe a) -> a -> a
asumOr ms def = fromMaybe def (asum ms)

asumL :: (Monad m, Foldable f) => f (MaybeT m a) -> m a -> m a
asumL ms def = do
  runMaybeT (asum ms) >>= \case
    Nothing -> def
    Just x -> pure x

runBoolT :: (Monad m) => MaybeT m a -> m Bool
runBoolT m = isJust <$> runMaybeT m

runBool :: Maybe a -> Bool
runBool = isJust

firstJustM :: (Functor f, Foldable f, Monad m) => f (m (Maybe a)) -> m (Maybe a)
firstJustM xs = runMaybeT $ asum $ fmap MaybeT xs

data TripleDelta k a b = TripleDelta
  { leftOnly :: HashMap k a
  , both :: HashMap k (a, b)
  , rightOnly :: HashMap k b
  }
  deriving (Show)

tripleDelta :: (Hashable k) => HashMap k a -> HashMap k b -> TripleDelta k a b
tripleDelta l r =
  TripleDelta
    { leftOnly = HashMap.difference l r
    , rightOnly = HashMap.difference r l
    , both = HashMap.intersectionWith (,) l r
    }

hashMapKeysEqual :: (Hashable k, Eq k) => HashMap k a -> HashMap k b -> Bool
hashMapKeysEqual m1 m2 = HashMap.keysSet m1 == HashMap.keysSet m2

{- | For example:

@
reorderToBe ['c', 'a', 'b'] ["apple", "banana", "carrot"] List.head
= ["carrot", "apple", "banana"]
@

If the inputs are invalid, this function returns 'Nothing'
-}
reorderByKeys :: (Hashable key, Eq key) => [key] -> [item] -> (item -> key) -> Maybe [item]
reorderByKeys keys items itemToKey = do
  let keyToIx = HashMap.fromList $ zip keys [0 ..]
  let numKeys = length keys
  let numItems = length items

  -- Keys must be unique
  unless (HashMap.size keyToIx == numKeys) empty

  -- Number of items and keys must be the same
  unless (numKeys == numItems) empty

  indices <- for items \item -> do
    HashMap.lookup (itemToKey item) keyToIx

  pure $ unsafeReorderByIndices indices items

{- | For example:

@ reorderByIndices [2, 0, 1] ["apple", "banana", "carrot"] = ["carrot", "apple",
"banana"] @

If the inputs are invalid, this function has undefined behavior.
-}
unsafeReorderByIndices :: [Int] -> [item] -> [item]
unsafeReorderByIndices indices items =
  [vec V.! i | i <- indices]
 where
  vec = V.fromList items

dupIORef :: (MonadIO m) => IORef a -> m (IORef a)
dupIORef ref = do
  val <- liftIO $ readIORef ref
  liftIO $ newIORef val

copyIORef :: (MonadIO m) => IORef a -> IORef a -> m ()
copyIORef from to = do
  val <- liftIO $ readIORef from
  liftIO $ writeIORef to val

-- | Takes a set and returns an arbitrary element from it, or Nothing if the set is empty.
hashSetChoose :: HashSet a -> Maybe a
hashSetChoose set = do
  -- NOTE: Hope that laziness makes this efficient even for large sets...
  listToMaybe $ HashSet.toList set