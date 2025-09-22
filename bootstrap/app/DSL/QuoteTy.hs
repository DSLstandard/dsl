module DSL.QuoteTy where

import DSL.EvalCxt
import DSL.EvalTy
import DSL.Types
import DSL.Utils
import Relude
import Utils.Misc (panic)

quoteTy :: EvalCxt -> TyLvl -> Vy -> IO Ty
quoteTy cxt = go
 where
  go :: TyLvl -> Vy -> IO Ty
  go lvl inTy = do
    inTy <- forceTyWeak cxt inTy
    case inTy of
      VyRigid h sp -> do
        sp <- traverse (go lvl) sp
        pure $ tyFoldApp (goHead lvl h) sp
      VyFlex m sp -> do
        sp <- traverse (go lvl) sp
        pure $ tyFoldApp (TyMeta m) sp
      VyTuple elems -> do
        elems <- traverse (go lvl) elems
        pure $ TyTuple elems
      VyDict dict -> do
        dict <- traverse (go lvl) dict
        pure $ TyDict dict
      VyArr icit dom cod -> do
        dom <- go lvl dom
        cod <- go lvl cod
        pure $ TyArr icit dom cod
      VyForall kind c -> do
        t <- evalTyClosure cxt c (vyBound lvl)
        t <- go (lvl + 1) t
        pure $ TyForall kind t
      VyLam kind c -> do
        t <- evalTyClosure cxt c (vyBound lvl)
        t <- go (lvl + 1) t
        pure $ TyLam kind t

  goHead :: TyLvl -> VyHead -> Ty
  goHead lvl = \case
    VyClass u -> TyClass u
    VyKnown k -> TyKnown k
    VyBound l -> do
      let i = lvl - l - 1
      if i < 0
        then panic $ "quoteHead: invalid de Bruijn index: " <> show l <> " (lvl = " <> show lvl <> ")"
        else TyVar i
