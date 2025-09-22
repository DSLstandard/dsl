module DSL.TraceDep where

import DSL.Database
import DSL.ScanTmDefs
import DSL.Types
import Data.HashSet qualified as HashSet
import Relude
import Utils.Graph qualified as G

{- | Given a seed def UID, returns a graph of all defs that the seed def depends
on. Contains cycles or self-loops if there are recursive defs.
-}
traceDefDep :: Database -> [UID] -> IO (G.Graph UID () ())
traceDefDep db seeds = do
  G.exploreSimpleIO seeds \uid -> do
    def <- queryDef db uid
    let deps = maybe HashSet.empty scanTmDefs def.tm
    pure (HashSet.toList deps)
