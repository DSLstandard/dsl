module DSL.ElabKind where

import DSL.Cxt
import DSL.Cxt qualified as Cxt
import DSL.PatUtils
import DSL.Types
import DSL.Utils
import DSL.Expr
import DSL.Pat qualified as P
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable
import Prettyprinter qualified as PP
import Relude
import DSL.MetaStore
import DSL.CxtUtils

elabKind :: Cxt -> Expr -> IO Kind
elabKind cxt self =
  matchOr self pat (makeAndMarkErrorKind cxt self "Bad kind expression")
 where
  pat =
    mconcat
      [ patArrow cxt
      , patStar cxt
      ]

-- * Elab [-> ...]

patArrow :: Cxt -> P.PExpr (IO Kind)
patArrow cxt =
  P.block_ "->" $
    ( \kindExprs -> do
        kinds <- for kindExprs \kindExpr -> do
          elabKind cxt kindExpr

        let doms = NonEmpty.init kinds
        let cod = NonEmpty.last kinds
        pure $ kindFoldMap doms cod
    )
      <$> P.someItem (P.label "<kind>" P.anyExpr)

-- * Elab @*@

patStar :: Cxt -> P.PExpr (IO Kind)
patStar _cxt =
  P.sym "*" $> pure KStar

-- * Elab utils

makeAndMarkErrorKind :: Cxt -> Expr -> PP.Doc Void -> IO Kind
makeAndMarkErrorKind cxt self msg = do
  Cxt.markExprError cxt self msg
  cxtFreshKindMeta cxt (MetaReason self msg)
