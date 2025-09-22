module DSL.PrettyTy where

import DSL.Database
import DSL.Dict qualified as Dict
import DSL.PrettyKind
import DSL.Types
import DSL.Utils
import DSL.PrettyUtils
import Data.Traversable
import Data.Vector qualified as V
import Prettyprinter qualified as PP
import Relude

prettyTy :: (HasCallStack) => Database -> Env Name -> Ty -> IO (PP.Doc ann)
prettyTy db env = \case
  TyVar i -> do
    let name = envIx i env
    pure $ PP.pretty name
  TyMeta (TyMetaId m) -> do
    pure $ "?" <> PP.pretty m
  ty@TyArr{} -> do
    let (doms, cod) = tyUnfoldArr' ty
    domDocs <-
      for doms \(icit, dom) -> do
        domDoc <- prettyTy db env dom
        case icit of
          Auto -> pure $ slistSep ["auto", domDoc]
          Expl -> pure domDoc
    codDoc <- prettyTy db env cod
    pure $ slistSep $ mconcat [["->"], domDocs, [codDoc]]
  ty@TyApp{} -> do
    let (f, xs) = tyUnfoldApp ty
    docs <- traverse (prettyTy db env) (f : xs)
    pure $ slistSep docs
  TyDef uid -> do
    name <- xentryName <$> queryEntry db uid
    pure $ PP.pretty name
  TyClass uid -> do
    name <- xentryName <$> queryEntry db uid
    pure $ PP.pretty name
  TyKnown k -> do
    pure $ PP.pretty (knownTypeName k)
  TyTuple elems -> do
    if V.null elems
      then do
        pure ","
      else do
        elems <- traverse (prettyTy db env) elems
        pure $ slistSep (";" : toList elems)
  TyDict dict -> do
    entryDocs <- for (Dict.toList dict) $ \(key, ty) -> do
      tyDoc <- prettyTy db env ty
      pure $ slistSep [PP.pretty key, tyDoc]
    pure $ slistSep ("Dict" : entryDocs)
  TyForall kind cod -> do
    let n :: Name = "#" <> show (envLvl env)
    cod <- prettyTy db (envPush n env) cod
    pure $ "∀(" <> PP.pretty n <> ": " <> prettyKind kind <> "). " <> cod
  TyLam kind cod -> do
    let n :: Name = "#" <> show (envLvl env)
    cod <- prettyTy db (envPush n env) cod
    pure $ "λ(" <> PP.pretty n <> ": " <> prettyKind kind <> "). " <> cod