{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant pure" #-}
module Utils.Graph where

import Control.Exception
import Data.HashSet qualified as HashSet
import Data.HashTable.IO qualified as H
import Optics
import Relude
import Utils.Misc (hashSetChoose)

data AdjEntry k w v = AdjEntry
  { key :: k
  , value :: v
  , outgoing :: H.CuckooHashTable k w
  }

{- | A mutable directed graph.

k: node key type, w: edge weight/value type, v: node value type
-}
newtype Graph k w v = Graph
  { adjList :: H.CuckooHashTable k (AdjEntry k w v)
  }

makeFieldLabelsNoPrefix ''AdjEntry
makeFieldLabelsNoPrefix ''Graph

newtype GraphError = GraphError Text
  deriving (Show)

instance Exception GraphError

new :: (MonadIO m) => m (Graph k w v)
new = liftIO do
  Graph <$> H.new

tryGetAdjEntry :: (Hashable k, MonadIO m) => Graph k w v -> k -> m (Maybe (AdjEntry k w v))
tryGetAdjEntry g key = liftIO do
  H.lookup g.adjList key

getAdjEntry :: (Show k, Hashable k, MonadIO m) => Graph k w v -> k -> m (AdjEntry k w v)
getAdjEntry g key = liftIO do
  maybeNode <- tryGetAdjEntry g key
  case maybeNode of
    Nothing -> throwIO $ GraphError $ "getAdjEntry: node " <> show key <> " not found in g"
    Just node -> pure node

getNode :: (Show k, Hashable k, MonadIO m) => Graph k w v -> k -> m v
getNode g key = liftIO do
  entry <- getAdjEntry g key
  pure entry.value

existsNode :: (Hashable k, MonadIO m) => Graph k w v -> k -> m Bool
existsNode g k = liftIO do
  isJust <$> tryGetAdjEntry g k

addNode :: (Show k, Hashable k, MonadIO m) => Graph k w v -> k -> v -> m ()
addNode g key value = liftIO do
  exists <- existsNode g key
  when exists do
    throwIO $ GraphError $ "addNode: node " <> show key <> " already exists in g"

  outgoing <- H.new
  H.insert g.adjList key AdjEntry{key, value, outgoing}

getNeighbors :: (Show k, Hashable k, MonadIO m) => Graph k w v -> k -> m [(k, w)]
getNeighbors g key = liftIO do
  entry <- getAdjEntry g key
  outgoingList <- H.toList entry.outgoing
  pure outgoingList

{- | Tries to add a node with the given key and value. If a node with the same
key already exists, does nothing and returns False. Otherwise, adds the node
and returns True.
-}
tryAddNode :: (Show k, Hashable k, MonadIO m) => Graph k w v -> k -> v -> m Bool
tryAddNode g key value = liftIO do
  exists <- existsNode g key

  unless exists do
    addNode g key value

  let success = not exists
  pure success

assertHasNode :: (Show k, Hashable k, MonadIO m) => Graph k w v -> k -> m ()
assertHasNode g key = liftIO do
  has <- existsNode g key
  unless has do
    throwIO $ GraphError $ "assertHasNode: node " <> show key <> " not found in g"

{- | Adds a directed edge from `from` to `to` with the given edge value. Returns
`Nothing` if either `from` or `to` does not exist in the g. Otherwise,
returns the updated g.
-}
addEdge :: (Show k, Hashable k, MonadIO m) => Graph k w v -> k -> k -> w -> m ()
addEdge g from to edge = liftIO do
  assertHasNode g from
  assertHasNode g to

  from <- getAdjEntry g from
  H.insert from.outgoing to edge

keysSet :: (Hashable k, MonadIO m) => Graph k w v -> m (HashSet k)
keysSet g = liftIO do
  entries <- H.toList g.adjList
  pure $ HashSet.fromList (fst <$> entries)

exploreSimpleIO :: forall k m. (MonadIO m, Hashable k, Show k) => [k] -> (k -> m [k]) -> m (Graph k () ())
exploreSimpleIO seeds getNeighbors = do
  g <- new

  visitedSetRef <- newIORef $ HashSet.empty @k
  let
    visit :: k -> m ()
    visit k = do
      visitedSet <- readIORef visitedSetRef
      unless (HashSet.member k visitedSet) do
        modifyIORef' visitedSetRef $ HashSet.insert k

        void $ tryAddNode g k ()

        neighbors <- getNeighbors k
        for_ neighbors \k' -> do
          void $ tryAddNode g k' ()

          addEdge g k k' ()
          visit k'

  for_ seeds \seed -> do
    visit seed

  pure g

{- | Toposorts a graph. Returns in order from source to sink. If the reachable
subgraph has circuits, the function would work fine but has no guarantee on the
order of nodes in the cycle.
-}
toposort :: (Show k, Show w, Hashable k, MonadIO m) => Graph k w v -> m [(k, v)]
toposort g = liftIO do
  unvisitedRef <- newIORef =<< keysSet g
  resultRef <- newIORef []

  let
    visit curr = do
      unvisited <- readIORef unvisitedRef
      when (HashSet.member curr unvisited) do
        modifyIORef' unvisitedRef $ HashSet.delete curr
        entry <- getAdjEntry g curr

        dependencies <- H.toList entry.outgoing
        for_ dependencies \(dep, _w) -> do
          visit dep

        modifyIORef' resultRef $ ((entry.key, entry.value) :)

    visitAll = do
      unvisited <- readIORef unvisitedRef
      case hashSetChoose unvisited of
        Nothing -> pass
        Just seed -> do
          visit seed
          visitAll

  visitAll

  result <- readIORef resultRef
  pure $ toList result
