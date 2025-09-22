module DSL.TravTm where

import DSL.Types
import Optics
import Relude

{- | Traverse a term, applying the given function to each subterm. The function
is given the current de Bruijn level and a way to recursively traverse the
current term's children.
-}
travTm ::
  forall m.
  (Monad m) =>
  -- | A function that takes the current de Bruijn level, the term being
  -- visited, and a default implementation for recursively visiting the term's
  -- children.
  (TmLvl -> Tm -> m Tm -> m Tm) ->
  Tm ->
  m Tm
travTm f = visit 0
 where
  visitClause :: TmLvl -> MatchClause -> m MatchClause
  visitClause lvl clause = do
    bodyTm <- visit (lvl + clause.patEnvLvl) clause.bodyTm
    guardTm <- traverse (visit (lvl + clause.patEnvLvl)) clause.guardTm
    pure $ clause & set #bodyTm bodyTm & set #guardTm guardTm

  visit :: TmLvl -> Tm -> m Tm
  visit lvl tm = f lvl tm $ case tm of
    TmVar{} -> pure tm
    TmDef{} -> pure tm
    TmLit{} -> pure tm
    TmClass uid choice value -> do
      TmClass uid choice <$> visit lvl value
    TmVec xs -> do
      TmVec <$> traverse (visit lvl) xs
    TmTuple xs -> do
      TmTuple <$> traverse (visit lvl) xs
    TmLam body -> do
      TmLam <$> visit (lvl + 1) body
    TmApp isTail f x -> do
      TmApp isTail <$> visit lvl f <*> visit lvl x
    TmLet values next -> do
      TmLet <$> traverse (visit lvl) values <*> visit (lvl + 1) next
    TmGetTupleIndex subj i -> do
      TmGetTupleIndex <$> visit lvl subj <*> pure i
    TmSetTupleIndex subj i value -> do
      TmSetTupleIndex <$> visit lvl subj <*> pure i <*> visit lvl value
    TmGetDictKey subj key -> do
      TmGetDictKey <$> visit lvl subj <*> pure key
    TmDict dict -> do
      TmDict <$> traverse (traverse (visit lvl)) dict
    TmIf cond then_ else_ -> do
      TmIf <$> visit lvl cond <*> visit lvl then_ <*> visit lvl else_
    TmError -> do
      pure TmError
    TmMatch match -> do
      scrutinee <- visit lvl match.scrutinee
      clauses <- traverse (visitClause lvl) match.clauses
      pure . TmMatch $
        match
          & set #scrutinee scrutinee
          & set #clauses clauses

-- | 'travTm' but run with an Identity monad.
mapTm :: (TmLvl -> Tm -> Tm -> Tm) -> Tm -> Tm
mapTm f =
  runIdentity . travTm \lvl tm def -> do
    pure (f lvl tm (runIdentity def))
