module DSL.UnifyUtils where

import DSL.Cxt
import DSL.Cxt qualified as Cxt
import DSL.CxtUtils
import DSL.EvalTy
import DSL.Types
import DSL.UnifyKind
import DSL.UnifyTy
import DSL.Expr
import Relude

unifyKindOrReport :: Cxt -> Expr -> Kind -> Kind -> IO Bool
unifyKindOrReport cxt self gotKind expKind = do
  ok <- isRight <$> tryUnifyKind cxt gotKind expKind
  unless ok do
    gotTyDoc <- cxtRenderKind cxt gotKind
    expTyDoc <- cxtRenderKind cxt expKind
    Cxt.markExprError cxt self $ "Has kind: " <> gotTyDoc <> "\nShould be: " <> expTyDoc
  pure ok

unifyTyOrReport :: Cxt -> Expr -> Vy -> Vy -> IO Bool
unifyTyOrReport cxt self gotTy expTy = do
  ok <- isRight <$> tryUnifyTy cxt gotTy expTy
  unless ok do
    gotTyDoc <- cxtRenderVy cxt =<< forceTy (eval cxt) gotTy
    expTyDoc <- cxtRenderVy cxt =<< forceTy (eval cxt) expTy
    Cxt.markExprError cxt self $ "Has type: " <> gotTyDoc <> "\nShould be: " <> expTyDoc
  pure ok
