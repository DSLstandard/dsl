module DSL.MarkTailCalls where

import DSL.Types
import Optics
import Relude

{- | Mark tail calls in a term by marking 'TmCall's in tail position with
'IsTail True'.

This function don't bother optimizing tail calls inside lambdas at tail
position. It is the responsibility of other parts of the compiler to ensure
these are already TCO-ed beforehand.
-}
markTailCalls :: Tm -> Tm
markTailCalls inTm = case inTm of
  -- Mark tail call
  TmApp _ fn args -> do
    TmApp (IsTail True) fn args
  -- Go closer to tail
  TmLet vals next -> do
    TmLet vals (markTailCalls next)
  TmIf cond onTrue onFalse -> do
    TmIf cond (markTailCalls onTrue) (markTailCalls onFalse)
  TmError -> do
    TmError
  TmMatch tree -> do
    TmMatch $ tree & over (#clauses % traversed % #bodyTm) markTailCalls
  -- Uninteresting cases
  TmVar{} -> inTm
  TmDef{} -> inTm
  TmLit{} -> inTm
  TmClass{} -> inTm
  TmTuple{} -> inTm
  TmVec{} -> inTm
  TmDict{} -> inTm
  TmGetDictKey{} -> inTm
  TmGetTupleIndex{} -> inTm
  TmSetTupleIndex{} -> inTm
  TmLam{} -> inTm -- NOTE: See comment of this function