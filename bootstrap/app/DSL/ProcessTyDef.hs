module DSL.ProcessTyDef where

import DSL.Cxt
import DSL.Database
import DSL.ElabKind
import DSL.ElabTy
import DSL.Pat qualified as P
import Relude

patTyDef :: P.PExpr (Cxt -> IO ())
patTyDef =
  P.block_ "tydef" $
    ( \name kindExpr tyExpr cxt -> do
        kind <- elabKind cxt kindExpr
        ty <- checkTy cxt tyExpr kind

        uid <- generateUID cxt.db
        insertEntry cxt.db $ XEntry'TyDef XTyDef{uid, name, kind, ty}
        addToNamespace cxt name uid
    )
      <$> P.item (P.label "<name>" P.anySym)
      <*> P.item (P.label "<kind>" P.anyExpr)
      <*> P.item (P.label "<type>" P.anyExpr)
