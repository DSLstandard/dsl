module DSL.Cxt where

import Colog qualified as C
import DSL.Database
import DSL.EvalCxt
import DSL.MetaStore
import DSL.Namespace (Namespace)
import DSL.Namespace qualified as Namespace
import DSL.Types
import DSL.Utils
import DSL.Expr
import DSL.Location
import DSL.Parser (SyntaxError)
import Data.HashTable.IO qualified as H
import Optics
import Prettyprinter qualified as PP
import Relude
import Utils.Vec qualified as Vec

data ExprError = ExprError
  { expr :: Expr
  , message :: PP.Doc Void
  }
  deriving (Show)

data Cxt = Cxt
  { db :: Database
  , -- Type/term scope
    tyScope :: TyScope
  , tmScope :: TmScope
  -- ^ (Expression reference for error reporting, the term for the "try"
  -- expression, the type of the "try" expression) -> elaboration result
  , autoBounds :: [TmLvl]
  , -- File environment
    namespace :: Namespace UID
  -- ^ Name resolution environment.
  , exports :: Namespace UID
  -- ^ Things to export from the current file.
  , resolveImportPath :: Text -> IO (FilePath, Dim)
  -- ^ How to resolve an import path to a file path. The input is the raw string in the import statement.
  , dimToNamespace :: H.BasicHashTable Dim (Namespace UID)
  , dimToSource :: H.BasicHashTable Dim Text
  , syntaxErrors :: Vec.Vec SyntaxError
  , -- Meta
    metas :: MetaStore
  , -- Error reporting
    exprErrors :: Vec.Vec ExprError
  , -- Others
    debugOut :: C.LogAction IO Text
  , debugging :: Bool
  }
makeFieldLabelsNoPrefix ''Cxt

data NewCxtParams = NewCxtParams
  { debugOut :: C.LogAction IO Text
  , debugging :: Bool
  , resolveImportPath :: Text -> IO (FilePath, Dim)
  }

newCxt :: NewCxtParams -> IO Cxt
newCxt params = do
  db <- newDatabase

  namespace <- Namespace.create
  exports <- Namespace.create
  syntaxErrors <- Vec.new
  dimToNamespace <- H.new
  dimToSource <- H.new

  metas <- newMetaStore
  exprErrors <- Vec.new

  pure
    Cxt
      { db = db
      , tyScope = scopeNil
      , tmScope = scopeNil
      , autoBounds = []
      , namespace = namespace
      , exports = exports
      , resolveImportPath = params.resolveImportPath
      , dimToNamespace = dimToNamespace
      , dimToSource = dimToSource
      , syntaxErrors
      , exprErrors
      , metas
      , debugging = params.debugging
      , debugOut = params.debugOut
      }

eval :: Cxt -> EvalCxt
eval cxt =
  EvalCxt
    { db = cxt.db
    , metas = cxt.metas
    }

-- * Scoping

cxtExtendTy :: TyScope -> Cxt -> Cxt
cxtExtendTy sub cxt = cxt & over #tyScope (scopeExtend sub)

cxtPushTm :: Name -> Vy -> Cxt -> Cxt
cxtPushTm name entry cxt = cxt & over #tmScope (scopePush name entry)

cxtExtendTm :: TmScope -> Cxt -> Cxt
cxtExtendTm sub cxt = cxt & over #tmScope (scopeExtend sub)

cxtAddAutoBound :: TmLvl -> Cxt -> Cxt
cxtAddAutoBound lvl cxt = cxt & over #autoBounds (lvl :)

-- * Namespace

-- | Like 'openNamespace' but for a single entry.
openName :: Cxt -> Name -> UID -> IO ()
openName cxt name uid = do
  Namespace.insert cxt.namespace name uid

exportName :: Cxt -> Name -> UID -> IO ()
exportName cxt name uid = do
  Namespace.insert cxt.exports name uid

-- | Open AND export a (Name, UID) pair in the current file.
addToNamespace :: Cxt -> Name -> UID -> IO ()
addToNamespace cxt name uid = do
  openName cxt name uid
  exportName cxt name uid

{- | For a name that is being declared in the current file, register its UID in
the
-}
declareName :: Cxt -> Bool -> Name -> UID -> IO ()
declareName cxt isPub name uid = do
  when isPub $ exportName cxt name uid
  openName cxt name uid

{- | Open a namespace into the current file (but not export them).

New names overwrite old names if they have the same key.
-}
openNamespace :: Cxt -> Namespace UID -> IO ()
openNamespace cxt ns = do
  Namespace.include cxt.namespace ns

lookupNamespace :: Cxt -> Name -> IO (Maybe UID)
lookupNamespace cxt name = do
  Namespace.lookup cxt.namespace name

-- * Intellisense

markExprError :: Cxt -> Expr -> PP.Doc Void -> IO ()
markExprError cxt err msg = do
  Vec.append cxt.exprErrors (ExprError err msg)
