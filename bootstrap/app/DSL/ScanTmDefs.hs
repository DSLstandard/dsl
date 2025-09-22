module DSL.ScanTmDefs where

import Control.Monad.ST qualified as ST
import DSL.Types
import Data.HashSet qualified as HashSet
import Data.STRef.Strict qualified as ST
import Relude

scanTmDefs :: Tm -> HashSet UID
scanTmDefs inTm = ST.runST do
  set <- ST.newSTRef HashSet.empty

  let
    register uid = ST.modifySTRef' set (HashSet.insert uid)

    go = \case
      TmVar _n -> do
        pass
      TmDef uid -> do
        register uid
      TmLam body -> do
        go body
      TmApp _isTail f x -> do
        go f
        go x
      TmGetTupleIndex obj _index -> do
        go obj
      TmSetTupleIndex obj _index val -> do
        go obj
        go val
      TmGetDictKey dict _key -> do
        go dict
      TmLet values next -> do
        for_ values go
        go next
      TmLit _lit -> do
        pass
      TmClass _classUID _memberID value -> do
        go value
      TmVec vec -> do
        for_ vec go
      TmTuple tuple -> do
        for_ tuple go
      TmDict dict -> do
        for_ dict $ \(_key, val) -> do
          go val
      TmIf cond onTrue onFalse -> do
        go cond
        go onTrue
        go onFalse
      TmMatch tree -> do
        go tree.scrutinee
        for_ tree.clauses $ \clause -> do
          go clause.bodyTm
          traverse_ go clause.guardTm
      TmError -> do
        pass

  go inTm
  ST.readSTRef set
