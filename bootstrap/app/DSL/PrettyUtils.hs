module DSL.PrettyUtils where

import Prettyprinter qualified as P
import Relude

slistSep :: (Foldable f) => f (P.Doc ann) -> P.Doc ann
slistSep items = P.brackets (P.hsep (toList items))
