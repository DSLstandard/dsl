module DSL.EvalTy where

import Control.Monad
import DSL.Database
import DSL.EvalCxt
import DSL.EvalKind
import DSL.MetaStore
import DSL.Types
import DSL.Utils
import Relude
import Utils.Misc

-- * Eval

vyAppSp :: EvalCxt -> Vy -> Spine Vy -> IO Vy
vyAppSp cxt f inSp =
  case f of
    VyFlex m sp -> pure $ VyFlex m (spineConcat sp inSp)
    VyRigid h sp -> pure $ VyRigid h (spineConcat sp inSp)
    VyLam _kind closure ->
      case spineUncons inSp of
        Nothing -> pure f
        Just (x, xs) -> do
          x <- evalTyClosure cxt closure x
          vyAppSp cxt x xs
    _ -> do
      if spineIsNil inSp
        then pure f
        else panic $ "vyAppSp: not a function (spine is not empty)"

vyApp :: EvalCxt -> Vy -> Vy -> IO Vy
vyApp cxt f x = vyAppSp cxt f (spineOne x)

evalTyClosure :: EvalCxt -> TyClosure -> Vy -> IO Vy
evalTyClosure cxt (TyClosure env ty) arg = do
  evalTy cxt (envPush arg env) ty

evalTy :: EvalCxt -> TyEnv -> Ty -> IO Vy
evalTy cxt env = \case
  TyVar i -> do
    pure $ envIx i env
  TyMeta m -> do
    pure $ vyMeta m
  TyArr icit dom cod -> do
    dom <- evalTy cxt env dom
    cod <- evalTy cxt env cod
    pure $ VyArr icit dom cod
  TyForall kind t -> do
    pure $ VyForall kind (TyClosure env t)
  TyLam kind t -> do
    pure $ VyLam kind (TyClosure env t)
  TyTuple elems -> do
    elems <- traverse (evalTy cxt env) elems
    pure $ VyTuple elems
  TyDict dict -> do
    dict <- traverse (evalTy cxt env) dict
    pure $ VyDict dict
  TyApp f x -> do
    f <- evalTy cxt env f
    x <- evalTy cxt env x
    vyApp cxt f x
  TyClass u -> do
    pure $ vyClass u
  TyDef u -> do
    def <- queryTyDef cxt.db u
    evalTy cxt envNil def.ty
  TyKnown k -> do
    pure $ vyKnown k

-- * Forcing

tryForceTyMeta :: EvalCxt -> TyMetaId -> Spine Vy -> IO (Maybe Vy)
tryForceTyMeta cxt m sp = do
  meta <- getTyMeta cxt.metas m
  case meta.solution of
    Nothing -> do
      pure Nothing
    Just f -> do
      Just <$> vyAppSp cxt f sp

forceTyWeak :: EvalCxt -> Vy -> IO Vy
forceTyWeak c vyIn = case vyIn of
  VyFlex m sp -> do
    tryForceTyMeta c m sp >>= \case
      Nothing -> pure vyIn
      Just vy -> forceTyWeak c vy
  _ -> pure vyIn

forceTy :: EvalCxt -> Vy -> IO Vy
forceTy cxt vyIn = do
  case vyIn of
    VyTuple elems -> do
      VyTuple <$> traverse (forceTy cxt) elems
    VyDict dict -> do
      VyDict <$> traverse (forceTy cxt) dict
    VyFlex m sp -> do
      tryForceTyMeta cxt m sp >>= \case
        Just vy -> do
          forceTy cxt vy
        Nothing -> do
          -- At least we will force the spine if the meta is unsolved
          sp <- traverse (forceTy cxt) sp
          pure $ VyFlex m sp
    VyRigid head sp -> do
      sp <- traverse (forceTy cxt) sp
      pure $ VyRigid head sp
    VyArr icit dom cod -> do
      dom <- forceTy cxt dom
      cod <- forceTy cxt cod
      pure $ VyArr icit dom cod
    VyForall kind closure -> do
      kind <- forceKind cxt kind
      pure (VyForall kind closure)
    VyLam kind closure -> do
      kind <- forceKind cxt kind
      pure (VyLam kind closure)
