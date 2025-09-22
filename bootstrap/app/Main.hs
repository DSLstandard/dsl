{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Main where

import Colog qualified
import DSL.Cxt
import DSL.ProcessFile
import DSL.Expr
import DSL.JS.CodeGen
import DSL.Location
import DSL.Parser
import Data.HashTable.IO qualified as H
import Data.Time qualified as Time
import Error.Diagnose qualified as D
import Options.Applicative qualified as O
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Relude
import System.Directory
import System.FilePath ((</>))
import Utils.Vec qualified as Vec

buildDiagnose :: [(Dim, Text)] -> [SyntaxError] -> [ExprError] -> D.Diagnostic (PP.Doc Void)
buildDiagnose sources syntaxErrors exprErrors = modDiag mempty
 where
  toDiagnosePosition :: Loc -> D.Position
  toDiagnosePosition (Loc dim (Span start stop)) = do
    D.Position (toDiagnosePos start) (toDiagnosePos stop) (toString dim.unDim)

  toDiagnosePos :: Pos -> (Int, Int)
  toDiagnosePos (Pos line col) = (line + 1, col + 1)

  syntaxErrorReports =
    syntaxErrors <&> \err ->
      D.Err Nothing "Syntax error" [(toDiagnosePosition err.loc, D.This err.message)] []

  exprErrorReports =
    exprErrors <&> \err ->
      D.Err Nothing "Expression error" [(toDiagnosePosition err.expr.loc, D.This err.message)] []

  reports = syntaxErrorReports <> exprErrorReports

  modDiag =
    mconcat . mconcat $
      [ reports <&> \report diag -> D.addReport diag report
      , sources <&> \(uri, source) diag -> D.addFile diag (toString uri.unDim) (toString source)
      ]

data CompileParams = CompileParams
  { srcDir :: FilePath
  , mainFile :: FilePath
  , outputJSPath :: FilePath
  , debugging :: Bool
  , jsForeignImportModule :: FilePath
  }

compile :: CompileParams -> IO ()
compile params = do
  programStartTime <- Time.getCurrentTime
  cxt <-
    newCxt $
      NewCxtParams
        { debugOut = Colog.LogAction \msg -> do
            now <- Time.getCurrentTime
            let elapsed = now `Time.diffUTCTime` programStartTime
            putText "["
            putStr (Time.formatTime Time.defaultTimeLocale "%6Es" elapsed)
            putText "] "
            putTextLn msg
        , debugging = params.debugging
        , resolveImportPath = \identifier -> do
            path <- makeAbsolute (params.srcDir </> toString identifier)
            let dim = Dim (toText path)
            pure (path, dim)
        }

  importFile cxt (toText params.mainFile)

  -- Report errors
  fileSources <- H.toList cxt.dimToSource
  exprErrors <- Vec.toList cxt.exprErrors
  syntaxErrors <- Vec.toList cxt.syntaxErrors
  let diag = buildDiagnose fileSources syntaxErrors exprErrors
  when (D.hasReports diag) do
    let diagdoc = D.prettyDiagnostic' D.WithUnicode (D.TabSize 2) diag
    PP.putDoc diagdoc

  -- Compile to JS
  jsCode <-
    codegenJS $
      CodegenJSParams
        { db = cxt.db
        , metas = cxt.metas
        , importModule = toText params.jsForeignImportModule
        }
  let outputSource = encodeUtf8 $ PP.renderStrict (PP.layoutSmart PP.defaultLayoutOptions jsCode)

  let outputPath = params.outputJSPath
  currentSource <-
    doesFileExist outputPath >>= \case
      True -> do
        currentSource <- readFileBS outputPath
        pure $ Just currentSource
      False -> do
        pure Nothing

  if currentSource == Just outputSource
    then do
      putTextLn $ "[!] No changes to JS output, skipping write to " <> toText outputPath
    else do
      putTextLn $ "[!] Writing JS to " <> toText outputPath <> "..."
      writeFileBS outputPath outputSource
      putTextLn $ "[!] Done"

dev :: IO ()
dev = do
  compile
    CompileParams
      { srcDir = "script"
      , -- , mainFile = "main.dsl"
        mainFile = "small.dsl"
      , outputJSPath = "js/src/generated.js"
      , jsForeignImportModule = "./dsl_foreign"
      , debugging = True
      }

main :: IO ()
main = do
  join $ O.execParser opts
 where
  opts =
    O.info
      (parser O.<**> O.helper)
      (O.fullDesc <> O.progDesc "DSL compiler")

  parser =
    O.hsubparser . mconcat $
      [ O.command
          "compile"
          ( O.info
              ( ( \srcDir mainFile outputJSPath jsForeignImportModule debugging -> do
                    compile
                      CompileParams
                        { srcDir
                        , mainFile
                        , outputJSPath
                        , jsForeignImportModule
                        , debugging
                        }
                )
                  <$> O.strOption (O.short 's' <> O.long "src-dir" <> O.metavar "DIR" <> O.help "Source directory for imports")
                  <*> O.strOption (O.short 'm' <> O.long "main-file" <> O.metavar "FILE" <> O.help "Main DSL file to compile")
                  <*> O.strOption (O.short 'o' <> O.long "output-js" <> O.metavar "FILE" <> O.help "Output path for generated JS file")
                  <*> O.strOption (O.short 'i' <> O.long "js-foreign-import" <> O.metavar "FILE" <> O.help "Path to the JS foreign import module" <> O.value "./dsl_foreign")
                  <*> O.switch (O.short 'd' <> O.long "debug" <> O.help "Enable debug mode")
              )
              (O.progDesc "Compile DSL")
          )
      ]
