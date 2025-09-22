module DSL.MetaStore where

import Colog qualified as C
import DSL.Types
import DSL.Expr
import Data.HashMap.Strict qualified as HashMap
import Optics
import Prettyprinter qualified as PP
import Relude
import Utils.Misc

data MetaReason = MetaReason
  { expr :: Expr
  -- ^ Associated expression
  , desc :: PP.Doc Void
  -- ^ Description
  }
  deriving (Show)

data KindMetaEntry = KindMetaEntry
  { id :: KindMetaId
  , solution :: Maybe Kind
  , reason :: MetaReason
  }
  deriving (Show)
makeFieldLabelsNoPrefix ''KindMetaEntry

data TyMetaEntry = TyMetaEntry
  { id :: TyMetaId
  , solution :: Maybe Vy
  , kind :: Kind
  , reason :: MetaReason
  }
  deriving (Show)
makeFieldLabelsNoPrefix ''TyMetaEntry

data MetaStore = MetaStore
  { freshMetaId :: IO Int
  , tyMetaMap :: IORef (HashMap TyMetaId TyMetaEntry)
  , kindMetaMap :: IORef (HashMap KindMetaId KindMetaEntry)
  }

data MetaStoreParams = MetaStoreParams
  { debugOut :: C.LogAction IO Text
  , debugging :: Bool
  }

newMetaStore :: IO MetaStore
newMetaStore = do
  metaIdCounter <- newIORef 0
  tyMetaMap <- newIORef HashMap.empty
  kindMetaMap <- newIORef HashMap.empty

  pure
    MetaStore
      { freshMetaId = atomicModifyIORef' metaIdCounter \m -> (m + 1, m)
      , tyMetaMap
      , kindMetaMap
      }

{- | Duplicate the map of metas. Meta ID counters are not duplicated, so the new
store will generate fresh IDs remain GLOBALLY unique across all instances.
-}
isolateMetaStore :: MetaStore -> IO MetaStore
isolateMetaStore store = do
  tyMetaMap <- dupIORef store.tyMetaMap
  kindMetaMap <- dupIORef store.kindMetaMap
  pure store{tyMetaMap, kindMetaMap}

createTyMeta :: MetaStore -> TyMetaEntry -> IO ()
createTyMeta store entry = do
  modifyIORef' store.tyMetaMap (HashMap.insert entry.id entry)

createKindMeta :: MetaStore -> KindMetaEntry -> IO ()
createKindMeta store entry = do
  modifyIORef' store.kindMetaMap (HashMap.insert entry.id entry)

getTyMeta :: MetaStore -> TyMetaId -> IO TyMetaEntry
getTyMeta store metaId = do
  tyMetaMap <- readIORef store.tyMetaMap
  case HashMap.lookup metaId tyMetaMap of
    Just entry -> pure entry
    Nothing -> panic $ "TyMetaId not found: " <> show metaId

getKindMeta :: MetaStore -> KindMetaId -> IO KindMetaEntry
getKindMeta store metaId = do
  kindMetaMap <- readIORef store.kindMetaMap
  case HashMap.lookup metaId kindMetaMap of
    Just entry -> pure entry
    Nothing -> panic $ "KindMetaId not found: " <> show metaId

setTyMetaSolution :: (HasCallStack) => MetaStore -> TyMetaId -> Vy -> IO ()
setTyMetaSolution store m solution = do
  meta <- getTyMeta store m
  when (isJust meta.solution) do
    panic $ "TyMetaId already has a solution: " <> show m

  modifyIORef' store.tyMetaMap (HashMap.adjust (#solution ?~ solution) m)

setKindMetaSolution :: (HasCallStack) => MetaStore -> KindMetaId -> Kind -> IO ()
setKindMetaSolution store m solution = do
  meta <- getKindMeta store m
  when (isJust meta.solution) do
    panic $ "KindMetaId already has a solution: " <> show m

  modifyIORef' store.kindMetaMap (HashMap.adjust (#solution ?~ solution) m)
