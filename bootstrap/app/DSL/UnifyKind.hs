module DSL.UnifyKind where

import Colog qualified
import Control.Exception qualified as Exception
import DSL.Cxt
import DSL.CxtUtils
import DSL.EvalKind
import DSL.Types
import DSL.Utils (scanKindMetaIds)
import Data.HashSet qualified as HashSet
import Relude

newtype UnifyKindError = UnifyKindError CallStack
  deriving (Show)

instance Exception UnifyKindError

tryUnifyKind :: Cxt -> Kind -> Kind -> IO (Either UnifyKindError ())
tryUnifyKind cxt lhs rhs = do
  when cxt.debugging do
    lhsDoc <- cxtRenderKind cxt lhs
    rhsDoc <- cxtRenderKind cxt rhs
    cxt.debugOut Colog.<& "[tryUnifyKind] "
      <> show lhsDoc
      <> " ~ "
      <> show rhsDoc

  Exception.try @UnifyKindError $ unifyKind cxt lhs rhs

unifyKind :: Cxt -> Kind -> Kind -> IO ()
unifyKind cxt k1 k2 = do
  k1 <- forceKindWeak (eval cxt) k1
  k2 <- forceKindWeak (eval cxt) k2
  case (k1, k2) of
    (KStar, KStar) -> do
      pass
    (KArr dom1 cod1, KArr dom2 cod2) -> do
      unifyKind cxt dom1 dom2
      unifyKind cxt cod1 cod2
    (KMeta m, rhs) -> do
      solve m rhs
    (lhs, KMeta m) -> do
      solve m lhs
    _ -> do
      failUnify
 where
  failUnify :: (HasCallStack) => IO a
  failUnify = do
    Exception.throwIO $ UnifyKindError callStack

  solve :: KindMetaId -> Kind -> IO ()
  solve m kind = do
    kind <- forceKind (eval cxt) kind

    -- Occurrence check
    when (HashSet.member m (scanKindMetaIds kind)) do
      failUnify

    cxtSetKindMetaSolution cxt m kind
