module DSL.PatUtils where

import DSL.Cxt
import DSL.Cxt qualified as Cxt
import DSL.Expr
import DSL.Pat qualified as P
import Relude

matchOrReport :: Cxt -> Expr -> P.PExpr a -> IO (Maybe a)
matchOrReport cxt expr pat = do
  case P.match expr pat of
    Right a -> do
      pure (Just a)
    Left err -> do
      for_ (P.perrorList err) \(refExpr, msg) -> do
        Cxt.markExprError cxt refExpr msg
      pure Nothing

matchOr :: Expr -> P.PExpr a -> a -> a
matchOr expr pat fallback = do
  fromRight fallback (P.match expr pat)