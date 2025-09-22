module DSL.JS.JSPretty where

import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Scientific
import Prettyprinter ((<+>))
import Prettyprinter qualified as P
import Relude

-- | A JS identifier/variable name
type JSVar = Text

indent :: Int
indent = 2

type JSExpr = P.Doc Void

-- | Safely show a JSON value as a JS literal.
privateJSValue :: A.Value -> JSExpr
privateJSValue value = P.pretty $ A.encodeToLazyText value

-- | Safely show a JSVar as a JS identifier.
jsIdent :: JSVar -> JSExpr
jsIdent = P.pretty

jsInt :: (A.ToJSON a, Integral a) => a -> JSExpr
jsInt int = privateJSValue (A.toJSON int)

jsStr :: Text -> JSExpr
jsStr text = privateJSValue (A.toJSON text)

jsBool :: Bool -> JSExpr
jsBool bool = privateJSValue (A.toJSON bool)

jsDecimal :: Scientific -> JSExpr
jsDecimal decimal = privateJSValue (A.toJSON decimal)

jsFloat :: Float -> JSExpr
jsFloat float = privateJSValue (A.toJSON float)

jsDouble :: Double -> JSExpr
jsDouble float = privateJSValue (A.toJSON float)

jsFn :: [JSVar] -> JSExpr -> JSExpr
jsFn (map P.pretty -> params) =
  jsBlock (jsTuple params <> " => {") "}"

jsAsyncFn :: [JSVar] -> JSExpr -> JSExpr
jsAsyncFn (map P.pretty -> params) =
  jsBlock ("async " <> jsTuple params <> " => {") "}"

jsIIFE :: JSExpr -> JSExpr
jsIIFE = jsBlock "(() => {" "})()"

jsAsyncIIFE :: JSExpr -> JSExpr
jsAsyncIIFE = jsBlock "(await (async () => {" "})())"

jsIf :: JSExpr -> JSExpr -> JSExpr -> JSExpr
jsIf cond onTrue onFalse = do
  P.vcat
    [ "if (" <> cond <> ") {"
    , P.indent indent onTrue
    , "} else {"
    , P.indent indent onFalse
    , "}"
    ]

jsTuple :: [JSExpr] -> JSExpr
jsTuple docs = P.parens $ P.sep $ P.punctuate "," docs

jsList :: [JSExpr] -> JSExpr
jsList docs = P.brackets $ P.sep $ P.punctuate "," docs

jsCaseDefault :: JSExpr -> JSExpr
jsCaseDefault body = do
  P.vcat
    [ "default: {"
    , P.indent indent body
    , "}"
    ]

jsCase :: JSExpr -> JSExpr -> JSExpr
jsCase pat body = do
  P.vcat
    [ "case " <> pat <> ": {"
    , P.indent indent body
    , "}"
    ]

jsBlock :: JSExpr -> JSExpr -> JSExpr -> JSExpr
jsBlock begin end body = do
  P.flatAlt
    ( P.vcat
        [ begin
        , P.indent indent body
        , end
        ]
    )
    ( P.hcat [begin, body, end]
    )

jsReturn :: JSExpr -> JSExpr
jsReturn value = "return " <> value <> ";"

jsArrIndex :: JSExpr -> Int -> JSExpr
jsArrIndex arr idx = arr <> "[" <> show idx <> "]"

jsAwait :: JSExpr -> JSExpr
jsAwait fnCall = P.parens ("await " <> fnCall)

jsFor :: JSExpr -> JSExpr -> JSExpr
jsFor forInner body = do
  jsBlock
    ("for (" <> forInner <> ") {")
    ")"
    (P.indent indent body)

jsCall :: JSExpr -> [JSExpr] -> JSExpr
jsCall jsFunc [] = do
  jsFunc <> "()"
jsCall jsFunc jsArgs = do
  jsBlock (jsFunc <> "(") ")" (P.hcat $ P.punctuate ", " jsArgs)

jsCallMethod :: JSExpr -> JSVar -> [JSExpr] -> JSExpr
jsCallMethod obj method = jsCall (obj <> "." <> P.pretty method)

jsNew :: JSExpr -> [JSExpr] -> JSExpr
jsNew constructor args = do
  "new" <+> jsCall constructor args

jsSwitch :: JSExpr -> [JSExpr] -> JSExpr
jsSwitch scrutinee cases = do
  P.vcat
    [ "switch (" <> scrutinee <> ") {"
    , P.indent indent (P.vcat cases)
    , "}"
    ]

-- | Like 'jsSwitch', but automatically gives a 'default' that throws an error.
jsSafeSwitch :: JSExpr -> [JSExpr] -> JSExpr
jsSafeSwitch scrutinee cases =
  jsSwitch scrutinee (cases <> [jsCaseDefault "throw new Error('Non-exhaustive switch case');"])

{- | Create a object with the given fields. The field order is PRESERVED to
guarantee JavaScript execution order.
-}
jsObject :: [(JSVar, JSExpr)] -> JSExpr
jsObject fields = do
  let fieldDocs = map (\(k, v) -> jsStr k <> ": " <> v) fields
  P.braces $ P.sep $ P.punctuate "," fieldDocs

jsObjectGet :: JSExpr -> JSExpr -> JSExpr
jsObjectGet obj key = obj <> "[" <> key <> "]"
