module DSL.InstanceResolution where

import Colog qualified
import Control.Monad.Search qualified as Search
import DSL.Cxt
import DSL.CxtUtils
import DSL.Database
import DSL.EvalTy
import DSL.MetaStore
import DSL.Namespace qualified as Namespace
import DSL.TyHasMeta
import DSL.Types
import DSL.UnifyTy
import DSL.Utils
import DSL.Expr
import Data.Traversable
import Relude
import Utils.Search qualified as Search
import Witherable

tryResolveInstance ::
  Cxt ->
  -- | Pins the location for error-reporting
  Expr ->
  -- | The type to make an instance of
  Vy ->
  -- | Returns 'Nothing' if no instance could be found
  IO (Maybe Tm)
tryResolveInstance cxt inExprRef inWantTy = do
  when cxt.debugging do
    inWantTyDoc <- cxtRenderVy cxt inWantTy
    cxt.debugOut Colog.<& "[tryResolveInstance] <<< Attempting to solve for type: "
      <> show inWantTyDoc

  sol <- fmap snd <$> Search.runSearchBestT (solveTm inWantTy)

  when cxt.debugging do
    inWantTyDoc <- cxtRenderVy cxt inWantTy
    cxt.debugOut Colog.<& "[tryResolveInstance] >>> Finished search for type: "
      <> show inWantTyDoc
      <> ". Success = "
      <> show (isJust sol)

  pure sol
 where
  solveTmApp :: Vy -> Search.SearchIO Tm -> Vy -> Search.SearchIO Tm
  solveTmApp appTy mkAppTm expRetTy = do
    appTy <- lift $ forceTyWeak (eval cxt) appTy
    case appTy of
      VyFlex{} -> do
        -- Usually if we encounter an unsolved meta, we abandon immediately.
        -- This is because it is usually a sign that the user did not provide
        -- enough type hints for the compiler.
        Search.abandon
      VyForall kind cod -> do
        -- Plug foralls
        t <- lift $ cxtFreshTyMeta cxt kind (MetaReason inExprRef "type hole for instance resolution")
        cod <- lift $ evalTyClosure (eval cxt) cod t
        solveTmApp cod mkAppTm expRetTy
      VyArr Auto dom cod -> do
        -- Plug auto args
        let
          mkResTm :: Search.SearchIO Tm
          mkResTm = do
            f <- mkAppTm
            x <- solveTm dom
            pure (tmApp f x)
        solveTmApp cod mkResTm expRetTy
      appTy -> do
        -- Unify immediately to gain type information so the auto 'solveTm's in
        -- 'mkAppTm' can better properly.
        abandonUnlessUnifies cxt appTy expRetTy
        mkAppTm

  solveTm :: Vy -> Search.SearchIO Tm
  solveTm wantTy = do
    abandonIfTypeIsUgly cxt wantTy
    delayOneStep

    case wantTy of
      VyTuple elemTys -> do
        -- Special case: For tuples, we automatically search for instances of
        -- each element (like Haskell).
        elems <- for (toList elemTys) \elemTy -> do
          solveTm elemTy
        pure (TmTuple elems)
      _ -> do
        let
          branches'global :: Search.SearchIO Tm
          branches'global = do
            defUID <- pickOne =<< getAutoDefs cxt
            def <- liftIO $ queryDef cxt.db defUID
            defTy <- liftIO $ evalTy (eval cxt) envNil def.ty
            solveTmApp defTy (pure (TmDef defUID)) wantTy

          decomposeTuple :: Tm -> Vy -> Search.SearchIO (Tm, Vy)
          decomposeTuple x t = do
            t <- lift $ forceTyWeak (eval cxt) t
            abandonIfIsFlex t
            case t of
              VyTuple elemTys -> do
                (i :: Int, elemTy) <- pickOne $ zip [0 ..] (toList elemTys)
                let elemTm = TmGetTupleIndex x i
                decomposeTuple elemTm elemTy
              _ -> do
                pure (x, t)

          branches'local :: Search.SearchIO Tm
          branches'local = do
            l :: TmLvl <- pickOne cxt.autoBounds
            let i :: TmIx = scopeLvl cxt.tmScope - l - 1
            let boundTm = TmVar i
            let (_, boundTy) = scopeAt' i cxt.tmScope
            (candTm, candTy) <- decomposeTuple boundTm boundTy
            abandonIfTypeIsUgly cxt candTy
            abandonUnlessUnifies cxt wantTy candTy
            pure candTm

        asum [branches'local, branches'global]

delayOneStep :: Search.SearchIO ()
delayOneStep = Search.cost' Search.unitCost

pickOne :: (Functor f, Foldable f) => f a -> Search.SearchIO a
pickOne items = asum (pure <$> items)

{- | Usually if we encounter an unsolved meta, we abandon immediately. This is
because it is usually a sign that the user did not provide enough type hints for
the compiler.

The caller has the responsibility of forcing the input type.
-}
abandonIfIsFlex :: Vy -> Search.SearchIO ()
abandonIfIsFlex t = do
  case t of
    VyFlex{} -> Search.abandon
    _ -> pass

abandonUnlessUnifies :: Cxt -> Vy -> Vy -> Search.SearchIO ()
abandonUnlessUnifies cxt lhs rhs = do
  result <- lift $ tryUnifyTy cxt lhs rhs
  when (isLeft result) Search.abandon

{- | If the input type is considered "ugly" (i.e. it has unsolved metas), we
abandon immediately. This is generally used to prevent the instance resolution
from attempting any further with us knowing that it will not work properly when
holes are present.
-}
abandonIfTypeIsUgly :: Cxt -> Vy -> Search.SearchIO ()
abandonIfTypeIsUgly cxt t = do
  yes <- lift $ doesTyHaveMeta cxt t
  when yes do
    Search.abandon

getAutoDefs :: (MonadIO m) => Cxt -> m [UID]
getAutoDefs cxt = liftIO do
  -- FIXME: OPTIMIZE
  ns <- Namespace.toEntries cxt.namespace
  forMaybe ns \(_n, uid) -> runMaybeT do
    XEntry'Def def <- lift $ queryEntry cxt.db uid
    guard $ def.isAuto
    pure uid
