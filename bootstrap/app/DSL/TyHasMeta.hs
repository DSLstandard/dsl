module DSL.TyHasMeta where

import DSL.Cxt
import DSL.EvalTy
import DSL.Types
import DSL.Utils
import Relude
import Utils.Misc

doesTyHaveMeta :: Cxt -> Vy -> IO Bool
doesTyHaveMeta cxt inTy = do
  hasNoHole <- runBoolT $ go (scopeLvl cxt.tyScope) inTy
  pure $ not hasNoHole
 where
  go :: Lvl -> Vy -> MaybeT IO ()
  go lvl ty = do
    ty <- lift $ forceTyWeak (eval cxt) ty
    case ty of
      VyFlex{} -> do
        empty
      VyRigid _ sp -> do
        traverse_ (go lvl) sp
      VyTuple elems -> do
        traverse_ (go lvl) elems
      VyDict dict -> do
        traverse_ (go lvl) dict
      VyArr _icit dom cod -> do
        go lvl dom
        go lvl cod
      VyForall _kind closure -> do
        e <- lift $ evalTyClosure (eval cxt) closure (vyBound lvl)
        go (lvl + 1) e
      VyLam _kind closure -> do
        e <- lift $ evalTyClosure (eval cxt) closure (vyBound lvl)
        go (lvl + 1) e