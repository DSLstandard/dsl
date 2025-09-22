module DSL.EvalCxt where

import DSL.Database
import DSL.MetaStore

data EvalCxt = EvalCxt
  { db :: Database
  , metas :: MetaStore
  }
