module DSL.CxtUtils where

import Colog qualified
import DSL.Cxt
import DSL.EvalKind
import DSL.EvalTy (forceTy)
import DSL.MetaStore
import DSL.MetaStore qualified as MetaStore
import DSL.PrettyKind
import DSL.PrettyTy
import DSL.QuoteTy
import DSL.Types
import DSL.Utils
import DSL.Expr
import Prettyprinter qualified as PP
import Relude

cxtRenderVy :: Cxt -> Vy -> IO (PP.Doc Void)
cxtRenderVy cxt t = do
  t <- forceTy (eval cxt) t
  t <- quoteTy (eval cxt) (scopeLvl cxt.tyScope) t
  prettyTy cxt.db (scopeNames cxt.tyScope) t

cxtRenderConstantVy :: Cxt -> Vy -> IO (PP.Doc Void)
cxtRenderConstantVy cxt t = do
  t <- forceTy (eval cxt) t
  t <- quoteTy (eval cxt) 0 t
  prettyTy cxt.db envNil t

cxtRenderKind :: Cxt -> Kind -> IO (PP.Doc Void)
cxtRenderKind cxt k = do
  k <- forceKind (eval cxt) k
  pure $ prettyKind k

-- * Generating metas

-- | Like 'MetaStore.createTyMeta' but also writes debug info.
cxtFreshTyMeta :: Cxt -> Kind -> MetaReason -> IO Vy
cxtFreshTyMeta cxt kind reason = do
  m <- TyMetaId <$> cxt.metas.freshMetaId

  let kind' = tyScopeFoldAsKArrs cxt.tyScope kind
  createTyMeta cxt.metas $
    TyMetaEntry
      { id = m
      , kind = kind'
      , reason
      , solution = Nothing
      }

  when cxt.debugging do
    kindDoc <- cxtRenderKind cxt kind
    cxt.debugOut Colog.<& "[freshTyMeta] ?"
      <> show (unTyMetaId m)
      <> " : "
      <> show kindDoc
      <> " @ "
      <> show reason.expr.loc
      <> " ... "
      <> show reason.desc

  pure $ VyFlex m (vyBoundSp 0 (scopeLvl cxt.tyScope))

-- | Like 'MetaStore.createKinMeta' but also writes debug info.
cxtFreshKindMeta :: Cxt -> MetaReason -> IO Kind
cxtFreshKindMeta cxt reason = do
  m <- KindMetaId <$> cxt.metas.freshMetaId

  createKindMeta cxt.metas $
    KindMetaEntry
      { id = m
      , solution = Nothing
      , reason = reason
      }

  when cxt.debugging do
    cxt.debugOut Colog.<& "[freshKindMeta] ?"
      <> show (unKindMetaId m)
      <> " @ "
      <> show reason.expr.loc
      <> " ... "
      <> show reason.desc

  pure (KMeta m)

-- * Solving metas

-- | Like 'MetaStore.setTyMetaSolution' but also writes debug info.
cxtSetTyMetaSolution :: (HasCallStack) => Cxt -> TyMetaId -> Vy -> IO ()
cxtSetTyMetaSolution cxt m solution = do
  when cxt.debugging do
    solDoc <- cxtRenderConstantVy cxt solution
    cxt.debugOut Colog.<& "[setTyMetaSolution] ?"
      <> show (unTyMetaId m)
      <> " := "
      <> show solDoc
  MetaStore.setTyMetaSolution cxt.metas m solution

-- | Like 'MetaStore.setKindMetaSolution' but also writes debug info.
cxtSetKindMetaSolution :: (HasCallStack) => Cxt -> KindMetaId -> Kind -> IO ()
cxtSetKindMetaSolution cxt m solution = do
  when cxt.debugging do
    cxt.debugOut Colog.<& "[setKindMetaSolution] ?"
      <> show (unKindMetaId m)
      <> " := "
      <> show (prettyKind solution)
  MetaStore.setKindMetaSolution cxt.metas m solution
