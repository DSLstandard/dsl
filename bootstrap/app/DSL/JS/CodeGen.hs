{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}
module DSL.JS.CodeGen (codegenJS, CodegenJSParams (..), JSCxt, newJSCxt) where

import DSL.Database
import DSL.MetaStore
import DSL.ScanTmDefs
import DSL.Types
import DSL.JS.JSPretty
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Functor.WithIndex
import Data.HashMap.Strict qualified as HashMap
import Data.HashTable.IO qualified as H
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Traversable
import NeatInterpolation
import Prettyprinter qualified as PP
import Relude
import Utils.Graph qualified as G
import Utils.Misc (findMapped, panic)
import Utils.Vec qualified as Vec
import Witherable

data JSCxt = JSCxt
  { db :: Database
  , metas :: MetaStore
  , varCounter :: IORef Int
  -- ^ For generating distinct JavaScript variable names.
  , varEnv :: Env JSVar
  -- ^ TmIx -> JSVar
  , encodeDefUIDCache :: H.BasicHashTable UID Text
  , isDefConstantCache :: H.BasicHashTable UID Bool
  }

cacheBacked :: (Hashable k) => H.BasicHashTable k v -> k -> IO v -> IO v
cacheBacked cache key fallback = do
  H.lookup cache key >>= \case
    Nothing -> do
      val <- fallback
      H.insert cache key val
      pure val
    Just cached -> do
      pure cached

-- | Encode UID as a function name.
encodeDefUID :: JSCxt -> UID -> IO Text
encodeDefUID cxt uid = do
  cacheBacked cxt.encodeDefUIDCache uid do
    name <- xentryName <$> queryEntry cxt.db uid
    let n = T.concatMap mapper name <> "_UID" <> show (unUID uid)
    pure n
 where
  mapper :: Char -> Text
  mapper ch =
    if Char.isAlphaNum ch then T.singleton ch else "_"

defForeignImport :: XDef -> Maybe Text
defForeignImport def = findMapped (T.stripPrefix "foreign-import:") def.annotations

defForeignExport :: XDef -> Maybe Text
defForeignExport def = findMapped (T.stripPrefix "foreign-export:") def.annotations

isDefConstant :: JSCxt -> UID -> IO Bool
isDefConstant cxt uid = do
  cacheBacked cxt.isDefConstantCache uid do
    def <- queryDef cxt.db uid

    let isForeignImport = Maybe.isJust $ defForeignImport def

    pure case def.tm of
      Just tm -> not isForeignImport && countLams tm == 0
      Nothing -> False

countLams :: Tm -> Int
countLams inTm = do
  let
    go :: Int -> Tm -> Int
    go argc = \case
      TmLam body -> go (argc + 1) body
      _ -> argc
  go 0 inTm

newJSCxt :: Database -> MetaStore -> IO JSCxt
newJSCxt db metas = do
  varCounter <- newIORef 0
  encodeDefUIDCache <- H.new
  isDefConstantCache <- H.new
  pure
    JSCxt
      { db
      , metas
      , varCounter
      , varEnv = envNil
      , encodeDefUIDCache
      , isDefConstantCache
      }

freshJSVar :: JSCxt -> IO JSVar
freshJSVar c = do
  i <- atomicModifyIORef' c.varCounter (\i -> (i + 1, i))
  pure $ "v" <> show i

freshJSVars :: JSCxt -> Int -> IO (Env JSVar)
freshJSVars c n = do
  envFromList <$> for [0 .. n - 1] \_ -> freshJSVar c

pushVarScope :: JSVar -> JSCxt -> JSCxt
pushVarScope var c = do
  c{varEnv = envPush var c.varEnv}

extendVarEnv :: Env JSVar -> JSCxt -> JSCxt
extendVarEnv vs cxt = cxt{varEnv = envExtend vs cxt.varEnv}

pushVarScopeMany :: [JSVar] -> JSCxt -> JSCxt
pushVarScopeMany vs cxt = foldl' (flip pushVarScope) cxt vs

preludeDocText :: Text
preludeDocText =
  [text|
    export class TmError extends Error {
      constructor(message = undefined) {
        super(message ?? "Encountered TmError")
        this.name = "TmError"
      }
    }

    export function $$apply1(fn, arg) {
      // '$$trampoline' is defined by foreign import
      return $$trampoline(fn(arg))
    }

    export function $$tailcall(fn, arg) {
      // 'TailCall' is defined by foreign import
      return new TailCall(fn, arg)
    }

    function $$create_constant_def(initialize_constant) {
      let initialized = false
      let cached
      return () => {
        if (!initialized) {
          cached = initialize_constant()
          initialized = true
        }
        return cached
      }
    }
  |]

compileTm :: JSCxt -> Tm -> IO JSExpr
compileTm cxt inTm = do
  {-
    NOTE:

    - Struct encoding: A flat list of fields in order of declaration. Accessed
      by index.

    - Tuple encoding: An actual JavaScript array of fields.

    - List encoding: An actual JavaScript array.

    - Dict encoding: An actual JavaScript object with string keys.

    - Class encoding: A 2-item JS array, [<member ID>, <value>]. Note that
      struct is just a class with a single member with value being a tuple
      storing every field.

      e.g.,
        [class-decl Maybe [type A *]]
        [enum-impl Maybe
          [member Some A] // Member ID = 0
          [member None]   // Member ID = 1
          [member SomethingElse [; A Bool]] // Member ID = 2
        ]

        // DSL: [None]                           -> JS: [0, []]
        // DSL: [Some "Hello"]                   -> JS: [1, ["Hello"]]
        // DSL: [SomethingElse [; "Yeah" false]] -> JS: [2, ["Yeah", false]]

    - Lambda encoding: An actual JS function.
  -}
  case inTm of
    TmVar i -> do
      pure $ jsIdent (envIx i cxt.varEnv)
    TmLam body -> do
      jsvar <- freshJSVar cxt
      bodyDoc <- compileTm (pushVarScope jsvar cxt) body
      pure $ jsFn [jsvar] (jsReturn bodyDoc)
    inTm@TmLet{} -> do
      compileLet cxt inTm
    TmIf cond onTrue onFalse -> do
      condDoc <- compileTm cxt cond
      onTrueDoc <- compileTm cxt onTrue
      onFalseDoc <- compileTm cxt onFalse
      pure $ jsIIFE (jsIf condDoc (jsReturn onTrueDoc) (jsReturn onFalseDoc))
    TmMatch match -> do
      compileMatch cxt match
    TmError -> do
      pure $ jsIIFE "throw new TmError()"

    -- Dict
    TmDict dict -> do
      entries <- for dict \(key, value) -> do
        value <- compileTm cxt value
        pure (key, value)
      pure $ jsObject entries
    TmGetDictKey dict key -> do
      dictDoc <- compileTm cxt dict
      pure $ jsObjectGet dictDoc (jsStr key)

    -- Object
    TmGetTupleIndex tuple i -> do
      objDoc <- compileTm cxt tuple
      pure $ jsArrIndex objDoc i
    TmSetTupleIndex tuple i val -> do
      objDoc <- compileTm cxt tuple
      valDoc <- compileTm cxt val
      pure $ jsCallMethod objDoc "with" [jsInt i, valDoc]

    -- Construction
    TmLit lit ->
      pure $ case lit of
        LString str -> do
          jsStr str
        LI32 i32 -> do
          jsInt i32
        LBool b -> do
          jsBool b
        LChar ch -> do
          jsStr (T.singleton ch)
        LBytes bs -> do
          let byteInts = jsList (jsInt <$> BS.unpack bs)
          PP.parens (jsNew "Uint8Array" [byteInts])
    TmVec elems -> do
      elemDocs <- traverse (compileTm cxt) elems
      pure $ jsList elemDocs
    TmTuple elems -> do
      elemDocs <- traverse (compileTm cxt) elems
      pure $ jsList elemDocs
    TmClass _classUID (MemberID memberID) value -> do
      valueDoc <- compileTm cxt value
      pure $ jsList [jsInt memberID, valueDoc]
    TmApp (IsTail isTail) f x -> do
      f <- compileTm cxt f
      x <- compileTm cxt x
      pure
        if isTail
          then jsCall "$tailcall" [f, x]
          else jsCall "$apply1" [f, x]
    TmDef uid -> do
      -- Top-level constants are weird because they are behind
      -- 'create_constant_def' for lazy initialization to accomodate
      -- potentially mutually-recursive top-level constant definitions.
      isConst <- isDefConstant cxt uid
      jsName <- encodeDefUID cxt uid
      pure
        if isConst
          then jsCall (jsIdent jsName) []
          else jsIdent jsName

compileMatch :: JSCxt -> MatchTree -> IO JSExpr
compileMatch cxt tree = do
  let rootLvl = envLvl cxt.varEnv

  bodyMap <-
    HashMap.fromList <$> for tree.clauses \clause -> do
      patEnv <- freshJSVars cxt clause.patEnvLvl
      let patSp = envToSp patEnv

      bodyCode <- compileTm (extendVarEnv patEnv cxt) clause.bodyTm
      let bodyFnCode = jsFn (toList patSp) (jsReturn bodyCode)
      bodyFnVar <- freshJSVar cxt

      pure (clause.id, (bodyFnVar, bodyFnCode))

  guardMap <-
    HashMap.fromList <$> forMaybe tree.clauses \clause -> runMaybeT do
      guardTm <- hoistMaybe $ clause.guardTm
      liftIO do
        patEnv <- freshJSVars cxt clause.patEnvLvl
        let patSp = envToSp patEnv

        guardCode <- compileTm (extendVarEnv patEnv cxt) guardTm
        let guardFnCode = jsFn (toList patSp) (jsReturn guardCode)
        guardFnVar <- freshJSVar cxt

        pure (clause.id, (guardFnVar, guardFnCode))

  let
    patLvl2Ix :: JSCxt -> PatLvl -> Ix
    patLvl2Ix cxt l = do
      let patLvl = envLvl cxt.varEnv - rootLvl
      let i = patLvl - l - 1
      i

    patLvl2Var :: JSCxt -> PatLvl -> JSVar
    patLvl2Var cxt l = let i = patLvl2Ix cxt l in envIx i cxt.varEnv

    visitNode :: JSCxt -> MatchNode -> IO JSExpr
    visitNode cxt = \case
      MatchNode'LeafUnhandled -> do
        pure $ jsIIFE "throw new Error('Match failure: unhandled pattern');"
      MatchNode'Leaf leaf -> do
        let guardFn = HashMap.lookup leaf.targetClauseID guardMap
        let (bodyFnVar, _) = Maybe.fromJust $ HashMap.lookup leaf.targetClauseID bodyMap
        let patArgs = toList (envToSp leaf.patEnv) <&> \l -> jsIdent (patLvl2Var cxt l)
        let callBody = jsCall (jsIdent bodyFnVar) patArgs
        case (guardFn, leaf.onGuardFail) of
          (Nothing, Nothing) -> do
            -- Case: the clause has no guard
            pure $ jsReturn callBody
          (Just (guardFnVar, _), Just onGuardFail) -> do
            -- Case: The clause has a guard
            let callGuard = jsCall (jsIdent guardFnVar) patArgs
            onGuardFailCode <- visitNode cxt onGuardFail
            pure $ jsIf callGuard (jsReturn callBody) onGuardFailCode
          _ -> do
            panic $ "Badly compiled 'MatchNode' with guard functions. Is the compiler's pattern matching logic implemented correctly?"
      MatchNode'BranchTuple l branch -> do
        elemsEnv <- freshJSVars cxt branch.numFields
        nextCode <- visitNode (extendVarEnv elemsEnv cxt) branch.next
        pure . PP.vcat $
          [ PP.vsep $
              imap
                (\i elemVar -> "const " <> jsIdent elemVar <> " = " <> jsArrIndex (jsIdent (patLvl2Var cxt l)) i <> ";")
                (toList (envToSp elemsEnv))
          , nextCode
          ]
      MatchNode'BranchClass l branches -> do
        let scrutMemberID = jsArrIndex (jsIdent (patLvl2Var cxt l)) 0
        let scrutValue = jsArrIndex (jsIdent (patLvl2Var cxt l)) 1
        cases <- for branches \branch -> do
          valueVar <- freshJSVar cxt
          nextCode <- visitNode (extendVarEnv (envOne valueVar) cxt) branch.next
          pure . jsCase (jsInt (unMemberID branch.memberID)) . PP.vcat $
            [ "const " <> jsIdent valueVar <> " = " <> scrutValue <> ";"
            , nextCode
            ]
        pure $ jsSafeSwitch scrutMemberID cases
      MatchNode'BranchBool l branches -> do
        cases <- for branches \branch -> do
          nextCode <- visitNode cxt branch.next
          pure $ jsCase (jsBool branch.value) nextCode

        pure $ jsSafeSwitch (jsIdent (patLvl2Var cxt l)) cases

  rootScrutCode <- compileTm cxt tree.scrutinee
  rootScrutVar <- freshJSVar cxt
  decisionTreeCode <- visitNode (pushVarScope rootScrutVar cxt) tree.root

  pure . jsIIFE . PP.vcat $
    [ PP.vcat $
        HashMap.elems bodyMap <&> \(bodyFnVar, bodyFnCode) -> do
          "const " <> jsIdent bodyFnVar <> " = " <> bodyFnCode <> ";"
    , PP.vcat $
        HashMap.elems guardMap <&> \(guardFnVar, guardFnCode) -> do
          "const " <> jsIdent guardFnVar <> " = " <> guardFnCode <> ";"
    , "const " <> jsIdent rootScrutVar <> " = " <> rootScrutCode <> ";"
    , decisionTreeCode
    ]

compileLet :: JSCxt -> Tm -> IO JSExpr
compileLet cxt inTm = do
  -- We unfold the binds into a list of statements and a final term and
  -- process in that form. This is to improve the readability (and likely
  -- also the performance) of the generated code.
  let
    unfoldLet :: Tm -> ([[Tm]], Tm)
    unfoldLet = \case
      (TmLet vals next) -> do
        let (rest, last) = unfoldLet next
        (vals : rest, last)
      tm -> ([], tm)

    (stmts, final) = unfoldLet inTm

    walkLets :: JSCxt -> [[Tm]] -> IO [JSExpr]
    walkLets cxt [] = do
      finalDoc <- compileTm cxt final
      pure [jsReturn finalDoc]
    walkLets cxt (inVals : rest) = do
      letVars <- for inVals \_ -> freshJSVar cxt
      let letCxt = pushVarScopeMany letVars cxt
      assignLines <- for (zip letVars inVals) \(letVar, val) -> do
        valJS <- compileTm letCxt val
        -- Use 'var' so JS hoists the declarations to the top of the function,
        -- and we need this to allow recursive lets to work.
        pure $ "var " <> jsIdent letVar <> " = " <> valJS <> ";"

      rests <- walkLets letCxt rest
      pure (assignLines <> rests)

  lines <- walkLets cxt stmts
  pure $ jsIIFE (PP.vcat lines)

data DefCode = DefCode
  { jsName :: JSVar
  , jsDefinition :: PP.Doc Void
  , postlude :: PP.Doc Void
  }

data CodegenJSParams = CodegenJSParams
  { db :: Database
  , metas :: MetaStore
  , importModule :: Text
  }

{- | Sorts definitions in topological order. Dependencies are placed before
co-dependencies.
-}
toposortDefs :: Database -> [UID] -> IO [UID]
toposortDefs db uids = do
  graph <- G.new
  for_ uids \uid -> do
    G.addNode graph uid ()
  for_ uids \self -> do
    def <- queryDef db self
    case def.tm of
      Nothing -> do
        pass
      Just tm -> do
        let dependencies = scanTmDefs tm
        for_ dependencies \dep -> do
          G.addEdge graph self dep ()
  out <- G.toposort graph
  pure $ reverse $ fmap fst $ out

codegenJS :: CodegenJSParams -> IO JSExpr
codegenJS CodegenJSParams{db, metas, importModule} = do
  uids <- listAllDefs db
  uids <- toposortDefs db uids
  cxt <- newJSCxt db metas

  imports <- Vec.new

  defCodes <- for uids \uid -> do
    def <- queryDef db uid

    jsName <- encodeDefUID cxt uid
    let mForeignImport = defForeignImport def
    let mForeignExport = defForeignExport def

    jsDefinition <- case (mForeignImport, def.tm) of
      (Nothing, Just tm) -> do
        tmDoc <- compileTm cxt tm
        isConst <- isDefConstant cxt def.uid
        if isConst
          then do
            pure $ "var " <> jsIdent jsName <> " = $create_constant_def(" <> jsFn [] (jsReturn tmDoc) <> ");"
          else do
            pure $ "var " <> jsIdent jsName <> " = " <> tmDoc <> ";"
      (Just foreignImport, Nothing) -> do
        Vec.append imports (foreignImport <> " as " <> jsName)
        pure mempty
      _ -> do
        panic $ "No implementation found for top-level definition with UID: " <> show uid

    pure
      DefCode
        { jsName = jsName
        , jsDefinition = jsDefinition
        , postlude = case mForeignExport of
            Nothing -> do
              mempty
            Just foreignExport -> do
              "export const " <> jsIdent foreignExport <> " = " <> jsIdent jsName <> ";"
        }

  imports <- Vec.toList imports

  let
    basicImportDoc = "import { $trampoline, TailCall } from " <> jsStr importModule
    foreignImportDoc = "import { " <> PP.hcat (PP.punctuate ", " (jsIdent <$> imports)) <> " } from " <> jsStr importModule

    defDefinitionsDoc =
      PP.vcat $
        defCodes <&> \defCode ->
          vacuous defCode.jsDefinition

    defPostludeDoc =
      PP.vcat $
        defCodes <&> \defCode -> do
          vacuous defCode.postlude

  pure $
    PP.vcat
      [ basicImportDoc
      , foreignImportDoc
      , PP.pretty preludeDocText
      , defDefinitionsDoc
      , defPostludeDoc
      ]
