module DSL.ProcessFile where

import Colog qualified
import DSL.Cxt
import DSL.Cxt qualified as Cxt
import DSL.Namespace qualified as Namespace
import DSL.PatUtils
import DSL.ProcessClass
import DSL.ProcessDef
import DSL.ProcessTyDef
import DSL.Expr
import DSL.Location
import DSL.Pat qualified as P
import DSL.Parser (ParseSourceResult (..), parseSource)
import Data.HashTable.IO qualified as H
import Data.Time qualified as Time
import Optics
import Relude
import Utils.Misc (impossible)
import Utils.Vec qualified as Vec

patImport :: P.PExpr (Cxt -> IO ())
patImport = do
  P.block_ "import" $
    ( \path cxt -> do
        importFile cxt path
    )
      <$> P.item (P.label "path" P.anyStr)

processDecl :: Cxt -> Expr -> IO ()
processDecl cxt self = do
  whenJustM (matchOrReport cxt self pat) \elab -> do
    elab cxt
 where
  pat =
    mconcat
      [ patImport
      , patClassDecl
      , patClassEnum
      , patClassStruct
      , patDefAnnotate
      , patDefDecl
      , patDefImpl
      , patTyDef
      ]

processFileExpr :: Cxt -> Dim -> Expr -> IO ()
processFileExpr cxt dim self = timedOrNot do
  cxt <- scopeFileContext cxt

  decls <- matchOrReport cxt self pat
  decls <- pure $ fromMaybe [] decls

  for_ decls \decl -> do
    processDecl cxt decl

  H.insert cxt.dimToNamespace dim cxt.exports
 where
  pat = P.block_ "file" (P.manyItem (P.label "<decl>" P.anyExpr))

  timedOrNot :: IO () -> IO ()
  timedOrNot action = do
    if cxt.debugging
      then do
        start <- Time.getCurrentTime
        action
        stop <- Time.getCurrentTime
        let diff = stop `Time.diffUTCTime` start
        cxt.debugOut Colog.<& "Processed " <> show dim <> " in: " <> show diff
      else do
        action

importFile :: Cxt -> Text -> IO ()
importFile cxt path = do
  (filePath, dim) <- cxt.resolveImportPath path
  H.lookup cxt.dimToNamespace dim >>= \case
    Just namespace -> do
      Cxt.openNamespace cxt namespace
    Nothing -> do
      source <- decodeUtf8 <$> readFileBS filePath
      H.insert cxt.dimToSource dim source

      let parseResult = parseSource dim source

      Vec.extend cxt.syntaxErrors parseResult.syntaxErrors

      fileCxt <- scopeFileContext cxt
      processFileExpr fileCxt dim parseResult.expr

      H.lookup fileCxt.dimToNamespace dim >>= \case
        Just namespace -> do
          Cxt.openNamespace cxt namespace
        Nothing -> do
          impossible

{- | Isolate name resolution and export handling for a file in a separate
context.
-}
scopeFileContext :: Cxt -> IO Cxt
scopeFileContext cxt = do
  namespace <- Namespace.create
  exports <- Namespace.create
  pure $ cxt & set #namespace namespace & set #exports exports