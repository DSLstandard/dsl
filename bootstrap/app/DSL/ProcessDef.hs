module DSL.ProcessDef where

import DSL.Cxt
import DSL.Cxt qualified as Cxt
import DSL.Database
import DSL.ElabTm
import DSL.ElabTy
import DSL.EvalTy
import DSL.Types
import DSL.Expr
import DSL.Pat qualified as P
import Data.HashSet qualified as HashSet
import Optics
import Prettyprinter qualified as PP
import Relude

patDefDecl :: P.PExpr (Cxt -> IO ())
patDefDecl =
  P.block_ "def-decl" $
    ( \isPub isAuto name tyExpr cxt -> do
        ty <- checkTy cxt tyExpr KStar

        uid <- generateUID cxt.db
        insertEntry cxt.db . XEntry'Def $
          XDef{uid, name, isAuto, ty, tm = Nothing, annotations = mempty}

        Cxt.declareName cxt isPub name uid
    )
      <$> P.modifier (P.sym "pub")
      <*> P.modifier (P.sym "auto")
      <*> P.item (P.label "<name>" P.anySym)
      <*> P.item (P.label "<type>" P.anyExpr)

patDefImpl :: P.PExpr (Cxt -> IO ())
patDefImpl =
  P.block_ "def-impl" $
    ( \(nameExpr, name) tmExpr cxt -> do
        whenJustM (refDefOrReport cxt nameExpr name) \def -> do
          tyVal <- evalTy (eval cxt) envNil def.ty
          tm <- checkTm cxt tmExpr tyVal
          insertEntry cxt.db $ XEntry'Def (def & set #tm (Just tm))
    )
      <$> P.item (P.label "<name>" (P.capture P.anySym))
      <*> P.item (P.label "<body>" P.anyExpr)

patDefAnnotate :: P.PExpr (Cxt -> IO ())
patDefAnnotate =
  P.block_ "def-annotate" $
    ( \(nameExpr, name) annotation cxt -> do
        whenJustM (refDefOrReport cxt nameExpr name) \def -> do
          insertEntry cxt.db $ XEntry'Def (def & over #annotations (HashSet.insert annotation))
    )
      <$> P.item (P.label "<name>" (P.capture P.anySym))
      <*> P.item (P.label "<annotation>" P.anyStr)

-- * Utils

refDefOrReport :: Cxt -> Expr -> Name -> IO (Maybe XDef)
refDefOrReport cxt nameExpr name = do
  lookupNamespace cxt name >>= \case
    Nothing -> do
      Cxt.markExprError cxt nameExpr $ "Unknown def declaration: " <> PP.pretty name
      pure Nothing
    Just uid -> do
      def <- queryDef cxt.db uid
      pure (Just def)
