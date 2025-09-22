module DSL.PrettyKind where

import DSL.Types
import DSL.Utils
import DSL.PrettyUtils
import Prettyprinter qualified as P
import Relude

prettyKind :: Kind -> P.Doc ann
prettyKind = \case
  KMeta (KindMetaId m) -> do
    "?" <> P.pretty m
  KStar -> do
    "*"
  kind@KArr{} -> do
    let (doms, cod) = kindUnfoldMap kind
    slistSep ("->" : map prettyKind (doms <> [cod]))
