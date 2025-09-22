module DSL.Database where

import DSL.Types
import DSL.Utils
import Data.HashTable.IO qualified as H
import Data.List qualified as List
import Data.Vector qualified as V
import Optics
import Relude
import Utils.Misc
import Witherable

data XDef = XDef
  { uid :: UID
  , name :: Name
  , isAuto :: Bool
  , annotations :: HashSet Text
  , ty :: Ty
  , tm :: Maybe Tm
  -- ^ 'Nothing' if not implemented.
  }
  deriving (Show)

xdefSimple :: UID -> Name -> Ty -> Tm -> XDef
xdefSimple uid name ty tm =
  XDef
    { uid
    , name
    , ty
    , tm = Just tm
    , isAuto = False
    , annotations = mempty
    }

data XTyDef = XTyDef
  { uid :: UID
  , name :: Text
  , kind :: Kind
  , ty :: Ty
  }
  deriving (Show)

data Member = Member
  { id :: MemberID
  , name :: Name
  , ty :: Ty
  -- ^ Scoped under 'generics' of the class.
  }
  deriving (Show)

data XClass = XClass
  { uid :: UID
  , name :: Name
  , generics :: TyScope
  , members :: V.Vector Member
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''XDef
makeFieldLabelsNoPrefix ''XTyDef
makeFieldLabelsNoPrefix ''Member
makeFieldLabelsNoPrefix ''XClass

classMemberByID :: XClass -> MemberID -> Maybe Member
classMemberByID xclass (MemberID id) =
  xclass.members V.!? id

classMemberByName :: XClass -> Name -> Maybe Member
classMemberByName xclass name =
  List.find (\member -> member.name == name) xclass.members

data XEntry
  = XEntry'Def XDef
  | XEntry'TyDef XTyDef
  | XEntry'Class XClass
  deriving (Show)

xentryUID :: XEntry -> UID
xentryUID = \case
  XEntry'Def xdef -> xdef.uid
  XEntry'TyDef xtydef -> xtydef.uid
  XEntry'Class xclass -> xclass.uid

xentryName :: XEntry -> Text
xentryName = \case
  XEntry'Def xdef -> xdef.name
  XEntry'TyDef xtydef -> xtydef.name
  XEntry'Class xclass -> xclass.name

data Database = Database
  { uidMap :: H.BasicHashTable UID XEntry
  , uidNonceCounter :: IORef Int
  }

newDatabase :: IO Database
newDatabase = do
  uidMap <- H.new
  uidNonceCounter <- newIORef 0
  pure $ Database{uidMap, uidNonceCounter}

generateUID :: Database -> IO UID
generateUID db = do
  atomicModifyIORef' db.uidNonceCounter \nonce -> (nonce + 1, UID nonce)

insertEntry :: Database -> XEntry -> IO ()
insertEntry db entry = do
  H.insert db.uidMap (xentryUID entry) entry

queryEntry :: Database -> UID -> IO XEntry
queryEntry db uid = do
  mEntry <- H.lookup db.uidMap uid
  case mEntry of
    Just entry -> pure entry
    Nothing -> panic $ "UID not found in database: " <> show uid

queryDef :: Database -> UID -> IO XDef
queryDef db uid = do
  entry <- queryEntry db uid
  case entry of
    XEntry'Def xdef -> pure xdef
    _ -> panic $ "Expected Def entry for UID: " <> show uid

queryTyDef :: Database -> UID -> IO XTyDef
queryTyDef db uid = do
  entry <- queryEntry db uid
  case entry of
    XEntry'TyDef xtydef -> pure xtydef
    _ -> panic $ "Expected TyDef entry for UID: " <> show uid

queryClass :: Database -> UID -> IO XClass
queryClass db uid = do
  entry <- queryEntry db uid
  case entry of
    XEntry'Class xclass -> pure xclass
    _ -> panic $ "Expected Class entry for UID: " <> show uid

listAllDefs :: Database -> IO [UID]
listAllDefs db = do
  list <- H.toList db.uidMap
  forMaybe list \(_, entry) -> runMaybeT do
    XEntry'Def def <- pure entry
    pure def.uid