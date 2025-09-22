module DSL.EvalKind where

import DSL.EvalCxt
import DSL.MetaStore
import DSL.Types
import Relude

-- * Forcing

tryForceKindMeta :: EvalCxt -> KindMetaId -> IO (Maybe Kind)
tryForceKindMeta cxt m = do
  m <- getKindMeta cxt.metas m
  pure m.solution

forceKindWeak :: EvalCxt -> Kind -> IO Kind
forceKindWeak cxt inKind = case inKind of
  KMeta m ->
    tryForceKindMeta cxt m >>= \case
      Nothing -> pure inKind
      Just kind -> forceKindWeak cxt kind
  _ -> pure inKind

forceKind :: EvalCxt -> Kind -> IO Kind
forceKind cxt starIn = case starIn of
  KStar -> pure KStar
  KArr s1 s2 -> KArr <$> forceKind cxt s1 <*> forceKind cxt s2
  KMeta m ->
    tryForceKindMeta cxt m >>= \case
      Nothing -> pure starIn
      Just star -> forceKind cxt star