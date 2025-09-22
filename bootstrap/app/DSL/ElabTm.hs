{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Redundant pure" #-}

module DSL.ElabTm (checkTm, inferTm, CheckTy (..)) where

import DSL.Cxt
import DSL.Cxt qualified as Cxt
import DSL.CxtUtils
import DSL.Database
import DSL.Dict qualified as Dict
import DSL.ElabKind
import DSL.ElabTy
import DSL.EvalTy
import DSL.InstanceResolution
import DSL.MarkTailCalls
import DSL.MetaStore
import DSL.PMatch (PMUserClause (PMUserClause))
import DSL.PMatch qualified as PMatch
import DSL.PatUtils
import DSL.QuoteTy
import DSL.Types
import DSL.UnifyTy
import DSL.UnifyUtils
import DSL.Utils
import DSL.Expr
import DSL.Pat qualified as P
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable
import Data.Vector qualified as V
import Optics
import Prettyprinter qualified as PP
import Relude
import Utils.Misc
import Witherable

data CheckTy = CheckTy Vy | InferTy
  deriving (Show)

elabTm :: Cxt -> Expr -> CheckTy -> IO (Tm, Vy)
elabTm cxt self check = do
  matchOr self pat (makeAndMarkErrorElabTm cxt self check "Bad term expression")
 where
  pat =
    mconcat
      [ patMatch cxt check
      , patTuple cxt check
      , patDo cxt check
      , patDict cxt check
      , patLam cxt check
      , patIf cxt check
      , patVec cxt check
      , P.list ((:|) <$> P.item P.anyExpr <*> P.rest) <&> \elems -> do
          elabFluent cxt self elems check
      , P.anyInt <&> \int -> do
          onInferredTm cxt self (TmLit (LI32 (fromIntegral int))) (vyKnown KnownType'I32) check
      , P.anyStr <&> \str -> do
          onInferredTm cxt self (TmLit (LString str)) (vyKnown KnownType'String) check
      , P.anyChar <&> \ch -> do
          onInferredTm cxt self (TmLit (LChar ch)) (vyKnown KnownType'Char) check
      , P.sym "," $> do
          onInferredTm cxt self (TmTuple []) (VyTuple mempty) check
      , P.sym "#" $> do
          -- Term hole for users to put temporary placeholders when coding
          holeTy <- case check of
            CheckTy expTy -> do
              pure expTy
            InferTy -> do
              cxtFreshTyMeta cxt KStar (MetaReason self "term hole")
          pure (TmError, holeTy)
      , P.label "<variable>" P.anySym <&> \name -> do
          elabVar cxt self name check
      ]

-- * Elab if

patIf :: Cxt -> CheckTy -> P.PExpr (IO (Tm, Vy))
patIf cxt check = do
  P.block_ "if" $
    ( \cond onTrue onFalse -> do
        cond <- checkTm cxt cond vyBool
        case check of
          InferTy -> do
            -- The true branch determines the type
            (onTrue, gotTy) <- inferTm cxt onTrue
            onFalse <- checkTm cxt onFalse gotTy
            pure (TmIf cond onTrue onFalse, gotTy)
          CheckTy expTy -> do
            onTrue <- checkTm cxt onTrue expTy
            onFalse <- checkTm cxt onFalse expTy
            pure (TmIf cond onTrue onFalse, expTy)
    )
      <$> P.item (P.label "<cond>" P.anyExpr)
      <*> P.item (P.label "<on-true>" P.anyExpr)
      <*> P.item (P.label "<on-false>" P.anyExpr)

-- * Elab do

data DoLetEntry = DoLetEntry
  { self :: Expr
  , name :: Name
  , value :: Expr
  , valueType :: Maybe Expr
  }
  deriving (Show)

patDo :: Cxt -> CheckTy -> P.PExpr (IO (Tm, Vy))
patDo cxt inCheck =
  P.block_ "do" $
    ( \stmts -> do
        elab stmts
    )
      <$> P.someItem (P.label "<stmt>" P.anyExpr)
 where
  elab :: NonEmpty Expr -> IO (Tm, Vy)
  elab inStmtExprs = do
    let
      stmtExprs = NonEmpty.init inStmtExprs
      lastExpr = NonEmpty.last inStmtExprs

      go :: [Expr] -> Cxt -> IO (Tm, Vy)
      go [] cxt = do
        elabTm cxt lastExpr inCheck
      go (stmt : restStmts) cxt = do
        let
          parse :: Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy)
          parse cxt checkRest = do
            result <-
              matchOrReport cxt stmt . mconcat $
                [ pOrdinaryLet
                , pRecursiveLet
                , pIfLet
                , pElseLet
                , pIfMatchLet
                , pElseMatchLet
                , pTry
                , pExpr
                ]
            case result of
              Nothing -> do
                -- Skip this statement for error-tolerance
                checkRest cxt
              Just parse -> do
                parse cxt checkRest

        parse cxt (go restStmts)

    go stmtExprs cxt

  elabLet :: Bool -> [DoLetEntry] -> Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy)
  elabLet isRecursive letEntries cxt checkRest = do
    letScope <-
      scopeFromPushMany <$> for letEntries \letEntry -> do
        valueType <-
          case letEntry.valueType of
            Just valueType -> do
              valueType <- checkTy cxt valueType KStar
              valueType <- evalTy (eval cxt) (vyBoundEnv (scopeLvl cxt.tyScope)) valueType
              pure valueType
            Nothing -> do
              cxtFreshTyMeta cxt KStar $
                MetaReason
                  { expr = letEntry.value
                  , desc = "let binding type placeholder"
                  }
        pure (letEntry.name, valueType)

    -- Trick to pick between recursive and non-recursive let.
    let letValueCxt = cxtExtendTm (if isRecursive then letScope else scopeNullifyNameMap letScope) cxt

    letVals <- for (zip (scopeToPushMany letScope) letEntries) \((_, valueType), letEntry) -> do
      checkTm letValueCxt letEntry.value valueType

    (rest, restTy) <- checkRest (cxtExtendTm letScope cxt)
    pure (TmLet letVals rest, restTy)

  pOrdinaryLet :: P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pOrdinaryLet = do
    P.block "let" $
      ( \name valueExpr P.BlockInfo{self} ->
          elabLet False $
            [ DoLetEntry
                { self
                , name
                , value = valueExpr
                , valueType = Nothing
                }
            ]
      )
        <$> P.item P.anySym
        <*> P.item P.anyExpr

  pRecursiveLet :: P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pRecursiveLet =
    P.block_ "let*" $
      ( \entryExprs cxt checkRest -> do
          letEntries <- forMaybe entryExprs \entryExpr -> runMaybeT do
            ((_nameExpr, name), valueTypeExpr, valueExpr) <-
              MaybeT . matchOrReport cxt entryExpr . P.list $
                (,,)
                  <$> P.item (P.capture P.anySym)
                  <*> P.optionalItem (P.block_ "type" (P.item (P.label "<type>" P.anyExpr)))
                  <*> P.item (P.label "<value>" P.anyExpr)

            pure
              DoLetEntry
                { self = entryExpr
                , name
                , value = valueExpr
                , valueType = valueTypeExpr
                }

          elabLet True letEntries cxt checkRest
      )
        <$> P.rest

  pIfElseLet :: Text -> Bool -> P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pIfElseLet leader isIfLet = do
    P.block_ leader $
      ( \patExpr guardExpr scrutExpr branchExpr cxt checkRest -> do
          (scrutTm, scrutTy) <- inferTm cxt scrutExpr

          (patScope, pat) <- PMatch.checkPatternExpr cxt scrutTy patExpr
          let patCxt = cxtExtendTm patScope cxt
          let restCxt = if isIfLet then cxt else patCxt
          let branchCxt = if isIfLet then patCxt else cxt

          (restTm, restTy) <- checkRest restCxt

          branchTm <- checkTm branchCxt branchExpr restTy
          guardTm <- for guardExpr \guardExpr -> checkTm patCxt guardExpr vyBool

          let
            onMatchClause =
              PMatch.PMUserClause
                { pat = pat
                , patScope = patScope
                , guardTm = guardTm
                , bodyTm = if isIfLet then branchTm else restTm
                }

            onMismatchClause =
              PMatch.PMUserClause
                { pat = PMatch.Pat patExpr PMatch.PIgnored
                , patScope = scopeNil
                , guardTm = Nothing
                , bodyTm = if isIfLet then restTm else branchTm
                }

          tmMatch <-
            PMatch.compilePatternMatching cxt $
              PMatch.CompilePatternMatchingParams
                { scrutineeTm = scrutTm
                , scrutineeTy = scrutTy
                , scrutineeExpr = scrutExpr
                , userClauses = [onMatchClause, onMismatchClause]
                }
          pure (tmMatch, restTy)
      )
        <$> P.item (P.label "<pat>" P.anyExpr)
        <*> P.optionalItem (P.block_ "where" (P.item (P.label "<guard>" P.anyExpr)))
        <*> P.item (P.label "<scrutinee>" P.anyExpr)
        <*> P.item (P.label "<on-match>" P.anyExpr)

  pIfLet :: P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pIfLet = pIfElseLet "if-let" True

  pElseLet :: P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pElseLet = pIfElseLet "else-let" False

  pIfOrElseMatchLet :: Text -> Bool -> P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pIfOrElseMatchLet leader isIfLet = do
    P.block_ leader $
      ( \primaryPExpr primaryGuardExpr scrutExpr clauseExprs cxt checkRest -> do
          (scrutTm, scrutTy) <- inferTm cxt scrutExpr

          (primaryPatScope, primaryPat) <- PMatch.checkPatternExpr cxt scrutTy primaryPExpr
          let primaryCxt = cxtExtendTm primaryPatScope cxt

          primaryGuardTm <- for primaryGuardExpr \primaryGuardExpr -> do
            checkTm primaryCxt primaryGuardExpr vyBool

          (restTm, restTy) <- checkRest primaryCxt

          let
            primaryUserClause =
              PMUserClause
                { pat = primaryPat
                , patScope = primaryPatScope
                , guardTm = primaryGuardTm
                , bodyTm = restTm
                }

          secondaryUserClauses <- forMaybe clauseExprs \clauseExpr -> runMaybeT do
            -- Skip bad patterns for error-tolerance
            (patExpr, guardExpr, bodyExpr) <-
              MaybeT . matchOrReport cxt clauseExpr . P.list $
                (,,)
                  <$> P.item (P.label "<pat>" P.anyExpr)
                  <*> P.optionalItem (P.block_ "where" (P.item (P.label "<guard>" P.anyExpr)))
                  <*> P.item (P.label "<body>" P.anyExpr)

            liftIO do
              (patScope, pat) <- PMatch.checkPatternExpr cxt scrutTy patExpr
              let secondaryCxt = cxtExtendTm patScope cxt

              -- Secondary body term's type is determined by whatever the rest
              -- of the "do" block is.
              bodyTm <- checkTm secondaryCxt bodyExpr restTy
              guardTm <- for guardExpr \guardExpr -> do
                checkTm secondaryCxt guardExpr vyBool

              pure $
                PMUserClause
                  { pat = pat
                  , patScope = patScope
                  , guardTm = guardTm
                  , bodyTm = bodyTm
                  }

          tmMatch <-
            PMatch.compilePatternMatching cxt $
              PMatch.CompilePatternMatchingParams
                { scrutineeTm = scrutTm
                , scrutineeTy = scrutTy
                , scrutineeExpr = scrutExpr
                , userClauses =
                    if isIfLet
                      then primaryUserClause : secondaryUserClauses
                      else
                        secondaryUserClauses <> [primaryUserClause]
                }

          pure (tmMatch, restTy)
      )
        <$> P.item (P.label "<pat>" P.anyExpr)
        <*> P.optionalItem (P.block_ "where" (P.item (P.label "<guard>" P.anyExpr)))
        <*> P.item (P.label "<scrutinee>" P.anyExpr)
        <*> P.manyItem (P.label "<clause>" P.anyExpr)

  pIfMatchLet :: P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pIfMatchLet = pIfOrElseMatchLet "let=" True

  pElseMatchLet :: P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pElseMatchLet = pIfOrElseMatchLet "else-let=" False

  -- Like the Haskell bind syntax '[try <name> <action>]' = 'do <name> <-
  -- <action>; ...'
  pTry :: P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pTry =
    P.block "try" $
      ( \name maExpr P.BlockInfo{self} cxt checkRest -> do
          -- Hand-craft an 'EApp' to simulate "[then maExpr [\ x rest]]"
          let
            app =
              EApp'ApplyTmArg
                ( EApp'ApplyTmArg
                    (EApp'Tm (elabVar cxt self "then" InferTy))
                    maExpr
                    (\check -> elabTm cxt maExpr check)
                )
                self
                ( \check -> do
                    check <- forceCheckTyWeak cxt check
                    case check of
                      CheckTy expTy@(VyArr Expl dom cod) -> do
                        -- Check if the type is nice
                        let restCxt = cxtPushTm name dom cxt
                        (restTm, restTy) <- checkRest restCxt

                        -- 'cod' should really match that of the 'check' of
                        -- 'elabTm', but we will unify with 'cod' anyway to
                        -- detect user mistyped 'then' functions.
                        ok <- unifyTyOrReport cxt self restTy cod
                        if ok
                          then do
                            pure (TmLam restTm, expTy)
                          else do
                            -- Drop the whole thing for error-tolerance.
                            pure (TmError, expTy)
                      _ -> do
                        -- Infer for 'check'
                        dom <- cxtFreshTyMeta cxt KStar (MetaReason self "[try ...] 'dom' type")

                        let restCxt = cxtPushTm name dom cxt
                        (restTm, restTy) <- checkRest restCxt

                        let inferredTy = VyArr Expl dom restTy
                        onInferredTm cxt self (TmLam restTm) inferredTy check
                )
          elabApp cxt self app inCheck
      )
        <$> P.item (P.label "<name>" P.anySym)
        <*> P.item (P.label "<value>" P.anyExpr)

  pExpr :: P.PExpr (Cxt -> (Cxt -> IO (Tm, Vy)) -> IO (Tm, Vy))
  pExpr = do
    P.anyExpr <&> \expr cxt checkRest -> do
      let letTy = vyUnit
      let letCxt = cxtPushTm "" letTy cxt
      letVal <- checkTm letCxt expr letTy
      (rest, restTy) <- checkRest letCxt
      pure (TmLet [letVal] rest, restTy)

-- * Elab variable

elabVar :: Cxt -> Expr -> Name -> CheckTy -> IO (Tm, Vy)
elabVar cxt self name check = do
  asumL
    [ do
        -- Try resolving as a local term variable
        (i, domTy) <- hoistMaybe $ scopeResolve name cxt.tmScope
        liftIO do
          onInferredTm cxt self (TmVar i) domTy check
    , do
        -- Try resolving as a def reference
        uid <- MaybeT $ lookupNamespace cxt name
        XEntry'Def def <- lift $ queryEntry cxt.db uid
        lift do
          ty <- evalTy (eval cxt) envNil def.ty
          onInferredTm cxt self (TmDef uid) ty check
    , do
        -- Try resolving as known constant values
        b :: Bool <-
          asum
            [ guard (name == "false") $> False
            , guard (name == "true") $> True
            ]
        lift do
          onInferredTm cxt self (TmLit (LBool b)) vyBool check
    ]
    ( do
        makeAndMarkErrorElabTm cxt self check ("Unknown term variable: '" <> PP.pretty name <> "'")
    )

-- * Elab lambda

patLam :: Cxt -> CheckTy -> P.PExpr (IO (Tm, Vy))
patLam cxt check = do
  P.block_ "\\" $
    ( \entries -> do
        elab entries
    )
      <$> P.someItem (P.label "<entry>" P.anyExpr)
 where
  elab :: NonEmpty Expr -> IO (Tm, Vy)
  elab inEntries = do
    let
      inParams = NonEmpty.init inEntries
      body = NonEmpty.last inEntries

      go :: [Expr] -> Cxt -> CheckTy -> IO (Tm, Vy)
      go [] cxt check = do
        elabTm cxt body check
      go (param : params) cxt check = do
        parse <- matchOrReport cxt param (patForall <> patParam <> patNameParam)
        case parse of
          Nothing -> do
            -- Skip bad lambda param for error-tolerance
            go params cxt check
          Just continue -> do
            continue cxt check (go params)

    go inParams cxt check

  elabParam :: (Expr, Icit, Name, Maybe Expr) -> Cxt -> CheckTy -> (Cxt -> CheckTy -> IO (Tm, Vy)) -> IO (Tm, Vy)
  elabParam (self, icit, name, dom'expr) cxt check doRestIn = do
    let
      isAuto = icit == Auto

      doRest :: Name -> Vy -> CheckTy -> IO (Tm, Vy)
      doRest name dom check = do
        cxt <- pure $ cxtPushTm name dom cxt
        cxt <- pure $ if isAuto then cxtAddAutoBound (scopeLvl cxt.tmScope - 1) cxt else cxt
        doRestIn cxt check

    check <- forceCheckTyWeak cxt check
    case check of
      CheckTy (VyArr exp'icit exp'dom exp'cod) | icit == exp'icit -> do
        case dom'expr of
          Nothing -> do
            pass
          Just dom'expr -> do
            t <- checkTy cxt dom'expr KStar
            t <- evalTy (eval cxt) (vyBoundEnv (scopeLvl cxt.tyScope)) t
            void $ unifyTyOrReport cxt dom'expr t exp'dom
        (rest'tm, _) <- doRest name exp'dom (CheckTy exp'cod)
        onCheckedTm (TmLam $ markTailCalls rest'tm) check
      _ -> do
        dom <- case dom'expr of
          Nothing -> do
            cxtFreshTyMeta cxt KStar (MetaReason self "type placeholder")
          Just dom'expr -> do
            t <- checkTy cxt dom'expr KStar
            t <- evalTy (eval cxt) (vyBoundEnv (scopeLvl cxt.tyScope)) t
            pure t
        (rest'tm, rest'ty) <- doRest name dom InferTy
        onInferredTm cxt self (TmLam $ markTailCalls rest'tm) (VyArr icit dom rest'ty) check

  patNameParam :: P.PExpr (Cxt -> CheckTy -> (Cxt -> CheckTy -> IO (Tm, Vy)) -> IO (Tm, Vy))
  patNameParam =
    (\(self, name) -> elabParam (self, Expl, name, Nothing))
      <$> P.label "<name>" (P.capture P.anySym)

  patParam :: P.PExpr (Cxt -> CheckTy -> (Cxt -> CheckTy -> IO (Tm, Vy)) -> IO (Tm, Vy))
  patParam =
    P.captures . P.list $
      ( \isAuto name typeExpr self ->
          elabParam (self, if isAuto then Auto else Expl, name, typeExpr)
      )
        <$> P.modifier (P.sym "auto")
        <*> P.item (P.label "<name>" P.anySym)
        <*> P.optionalItem (P.label "<type>" P.anyExpr)

  patForall :: P.PExpr (Cxt -> CheckTy -> (Cxt -> CheckTy -> IO (Tm, Vy)) -> IO (Tm, Vy))
  patForall =
    P.block "type" $
      ( \name kindExpr P.BlockInfo{self} cxt check doRest -> do
          got'kind <- case kindExpr of
            Nothing -> do
              cxtFreshKindMeta cxt MetaReason{expr = self, desc = "kind placeholder"}
            Just kindExpr -> do
              elabKind cxt kindExpr

          check <- forceCheckTyWeak cxt check
          case check of
            CheckTy (VyForall exp'kind exp'cod) -> do
              void $ unifyKindOrReport cxt self got'kind exp'kind
              exp'cod <- evalTyClosure (eval cxt) exp'cod (vyBound (scopeLvl cxt.tyScope))
              (tm, _) <- doRest (cxtExtendTy (scopeOne name exp'kind) cxt) (CheckTy exp'cod)
              onCheckedTm tm check
            check -> do
              (tm, rest'ty) <- doRest (cxtExtendTy (scopeOne name got'kind) cxt) InferTy
              rest'ty <- quoteTy (eval cxt) (scopeLvl cxt.tyScope + 1) rest'ty
              let final'ty = VyForall got'kind (TyClosure (vyBoundEnv (scopeLvl cxt.tyScope)) rest'ty)
              onInferredTm cxt self tm final'ty check
      )
        <$> P.item (P.label "<name>" P.anySym)
        <*> P.optionalItem (P.label "<kind>" P.anyExpr)

-- * Elab dict

patDict :: Cxt -> CheckTy -> P.PExpr (IO (Tm, Vy))
patDict cxt check = do
  P.block "dict" $ elabDict <$> P.rest
 where
  elabDict :: [Expr] -> P.BlockInfo -> IO (Tm, Vy)
  elabDict entries P.BlockInfo{self} = do
    entries <- forMaybe entries $ \entry -> matchOrReport cxt entry patEntry

    check <- forceCheckTyWeak cxt check
    let
      getCheckOfField = case check of
        CheckTy (VyDict fieldChecks) -> \key -> do
          maybe InferTy CheckTy (Dict.lookup key fieldChecks)
        _ -> \_key -> do
          InferTy

    (tms, tys) <-
      unzip <$> for entries \(key, valueExpr) -> do
        let fieldCheck = getCheckOfField key
        (x, t) <- elabTm cxt valueExpr fieldCheck
        pure ((key, x), (key, t))

    let gotTy = VyDict (Dict.fromList tys)
    let gotTm = TmDict tms
    onInferredTm cxt self gotTm gotTy check

  patEntry :: P.PExpr (Name, Expr)
  patEntry =
    P.list $
      (,)
        <$> P.item (P.label "<key>" P.anySym)
        <*> P.item (P.label "<value>" P.anyExpr)

-- * Elab tuple

patTuple :: Cxt -> CheckTy -> P.PExpr (IO (Tm, Vy))
patTuple cxt check = do
  P.block ";" (elabTuple <$> P.rest)
 where
  elabTuple :: [Expr] -> P.BlockInfo -> IO (Tm, Vy)
  elabTuple elems P.BlockInfo{self} = do
    check <- forceCheckTyWeak cxt check
    case check of
      -- Leverage the checked type whenever possible
      CheckTy (VyTuple elemTys) | length elemTys == length elems -> do
        elems <- for (zip elems (toList elemTys)) \(elem, elemTy) -> do
          checkTm cxt elem elemTy
        onCheckedTm (TmTuple elems) check
      _ -> do
        (elems, elemTys) <- unzip <$> for elems \elem -> inferTm cxt elem
        onInferredTm cxt self (TmTuple elems) (VyTuple (V.fromList elemTys)) check

-- * Elab vec

patVec :: Cxt -> CheckTy -> P.PExpr (IO (Tm, Vy))
patVec cxt check = do
  P.block "vec" (processElems <$> P.rest)
 where
  processElems :: [Expr] -> P.BlockInfo -> IO (Tm, Vy)
  processElems elems P.BlockInfo{self} = do
    check <- forceCheckTyWeak cxt check
    case check of
      CheckTy (VyRigid (VyKnown KnownType'Vec) (spineToList -> [itemType])) -> do
        -- Leverage the checked type whenever possible
        elems <- for elems \elem -> do
          checkTm cxt elem itemType
        onCheckedTm (TmTuple elems) check
      _ -> do
        itemType <- cxtFreshTyMeta cxt KStar MetaReason{expr = self, desc = "Vec item type hole"}
        elems <- for elems \elem -> do
          checkTm cxt elem itemType
        let vecType = VyRigid (VyKnown KnownType'Vec) (spineFromList [itemType])
        onInferredTm cxt self (TmTuple elems) vecType check

-- * Pattern matching constructs

-- | Elab [match ...]
patMatch :: Cxt -> CheckTy -> P.PExpr (IO (Tm, Vy))
patMatch cxt check = do
  P.block "match" $
    elabMatch
      <$> P.item (P.label "<scrutinee>" P.anyExpr)
      <*> P.rest
 where
  elabMatch :: Expr -> [Expr] -> P.BlockInfo -> IO (Tm, Vy)
  elabMatch scrutExpr clauseExprs block = do
    (scrutTm, scrutTy) <- inferTm cxt scrutExpr

    returnTy <- case check of
      CheckTy returnTy -> do
        pure returnTy
      InferTy -> do
        cxtFreshTyMeta cxt KStar MetaReason{expr = block.self, desc = "match return type hole"}

    -- NOTE: Invalid clause expressions are skipped for error-tolerance.
    userClauses <- forMaybe clauseExprs \clauseExpr -> runMaybeT do
      (patExpr, guardExpr, bodyExpr) <-
        MaybeT . matchOrReport cxt clauseExpr . P.list $
          (,,)
            <$> P.item (P.label "<pat>" P.anyExpr)
            <*> P.optionalItem (P.block_ "where" (P.item (P.label "<guard>" P.anyExpr)))
            <*> P.item (P.label "<value>" P.anyExpr)

      lift do
        (patScope, pat) <- PMatch.checkPatternExpr cxt scrutTy patExpr
        bodyTm <- checkTm (cxtExtendTm patScope cxt) bodyExpr returnTy
        guardTm <- for guardExpr \guardExpr -> checkTm (cxtExtendTm patScope cxt) guardExpr vyBool
        pure PMatch.PMUserClause{pat, patScope, guardTm, bodyTm}

    tmMatch <-
      PMatch.compilePatternMatching cxt $
        PMatch.CompilePatternMatchingParams
          { scrutineeTm = scrutTm
          , scrutineeTy = scrutTy
          , scrutineeExpr = scrutExpr
          , userClauses = userClauses
          }

    pure (tmMatch, returnTy)

-- * Elab application

data EApp
  = EApp'Tm (IO (Tm, Vy))
  | EApp'CallFn EApp Expr
  | EApp'ApplyTmArg EApp Expr (CheckTy -> IO (Tm, Vy))
  | EApp'ApplyTyArg EApp Expr
  | EApp'GetDictKey EApp (Expr, Name)

data ApplyHoles = ApplyHoles
  { appendForall :: Bool
  , appendAuto :: Bool
  }
  deriving (Show)

elabApp :: Cxt -> Expr -> EApp -> CheckTy -> IO (Tm, Vy)
elabApp cxt inExprRef inApp inCheck = do
  (mkAppTm, appTy) <- checkApp inApp
  (mkAppTm, appTy) <- applyHoles ApplyHoles{appendForall = True, appendAuto = True} inExprRef mkAppTm appTy
  case inCheck of
    CheckTy exp'retTy -> do
      -- We immediately unify the final application type with the expected
      -- return type. This way, we could infer types of the arguments much
      -- better
      ok <- unifyTyOrReport cxt inExprRef appTy exp'retTy
      if ok
        then do
          appTm <- mkAppTm
          pure (appTm, exp'retTy)
        else do
          -- If the application's type is bad, drop the whole thing for
          -- error-tolerance.
          pure (TmError, exp'retTy)
    InferTy -> do
      appTm <- mkAppTm
      pure (appTm, appTy)
 where
  checkApp :: EApp -> IO (IO Tm, Vy)
  checkApp (EApp'Tm infer) = do
    (tm, ty) <- infer
    pure (pure tm, ty)
  checkApp (EApp'ApplyTmArg fn exprRef elabArgTm) = do
    -- FIXME: Bad expr ref
    (mkFnTm, fnTy) <- checkApp fn
    (mkFnTm, fnTy) <- applyHoles ApplyHoles{appendForall = True, appendAuto = True} exprRef mkFnTm fnTy
    applyArg exprRef mkFnTm fnTy $ \check -> do
      (argTm, argTy) <- elabArgTm check
      pure (pure argTm, argTy)
  checkApp (EApp'CallFn app fn) = do
    -- FIXME: Bad expr ref
    let exprRef = fn
    (fnTm, fnTy) <- inferTm cxt fn
    (mkFnTm, fnTy) <- applyHoles ApplyHoles{appendForall = True, appendAuto = True} exprRef (pure fnTm) fnTy
    applyArg exprRef mkFnTm fnTy $ \check -> do
      (argTm, argTy) <- elabApp cxt exprRef app check
      pure (pure argTm, argTy)
  checkApp (EApp'ApplyTyArg app tyArg) = do
    -- FIXME: Bad expr ref
    let exprRef = tyArg
    (mkAppTm, appTy) <- checkApp app
    (mkAppTm, appTy) <- applyHoles ApplyHoles{appendForall = False, appendAuto = True} tyArg mkAppTm appTy

    appTy <- forceTyWeak (eval cxt) appTy
    case appTy of
      VyForall kind cod -> do
        -- Check if type is nice
        tyArg <- checkTy cxt tyArg kind
        tyArg <- evalTy (eval cxt) (vyBoundEnv (scopeLvl cxt.tyScope)) tyArg
        cod <- evalTyClosure (eval cxt) cod tyArg
        pure (mkAppTm, cod)
      _ -> do
        -- Infer for 'fnTy'
        (_tyArg, tyArgKind) <- inferTy cxt tyArg

        cod <- cxtFreshTyMeta cxt KStar MetaReason{expr = exprRef, desc = "result type hole for inferred application"}
        cod' <- quoteTy (eval cxt) (scopeLvl cxt.tyScope) cod
        cod' <- pure $ TyClosure (vyBoundEnv (scopeLvl cxt.tyScope)) cod'
        let inferredAppTy = VyForall tyArgKind cod'

        ok <- unifyTyOrReport cxt exprRef appTy inferredAppTy
        if ok
          then do
            pure (mkAppTm, cod)
          else do
            -- Drop this type arg for error-tolerance
            pure (mkAppTm, appTy)
  checkApp (EApp'GetDictKey app (nameExprRef, name)) = do
    -- FIXME: Bad expr ref
    let exprRef = nameExprRef
    (mkAppTm, appTy) <- checkApp app
    (mkAppTm, appTy) <- applyHoles ApplyHoles{appendForall = True, appendAuto = True} exprRef mkAppTm appTy

    let
      onCannotGetDictKey = do
        appTyDoc <- cxtRenderVy cxt appTy
        markExprError cxt nameExprRef $ "Cannot get field '" <> PP.pretty name <> "' from type: " <> appTyDoc

        -- Stop getting field for error-tolerance.
        pure (mkAppTm, appTy)

    appTy <- forceTyWeak (eval cxt) appTy
    case appTy of
      VyDict dict -> do
        case Dict.lookup name dict of
          Nothing -> do
            onCannotGetDictKey
          Just valueType -> do
            let
              mkResTm = do
                object <- mkAppTm
                pure (TmGetDictKey object name)
            pure (mkResTm, valueType)
      _ -> do
        onCannotGetDictKey

  applyArg :: Expr -> IO Tm -> Vy -> (CheckTy -> IO (IO Tm, Vy)) -> IO (IO Tm, Vy)
  applyArg exprRef mkFnTm fnTy elabArg = do
    fnTy <- forceTyWeak (eval cxt) fnTy
    case fnTy of
      VyArr Expl dom cod -> do
        -- Check if type is nice
        let
          mkResTm = do
            f <- mkFnTm

            -- NOTE: MUST BE AFTER 'mkFnTm'. 'mkFnTm' may solve metas for 'dom'.
            (mkArgTm, _argTy) <- elabArg (CheckTy dom)
            x <- mkArgTm

            pure (tmApp f x)
        pure (mkResTm, cod)
      _ -> do
        -- Infer for 'fnTy'
        (mkArgTm, argTy) <- elabArg InferTy
        resTy <- cxtFreshTyMeta cxt KStar MetaReason{expr = exprRef, desc = "result type hole for inferred application"}

        ok <- unifyTyOrReport cxt exprRef fnTy (VyArr Expl argTy resTy)
        if ok
          then do
            let
              mkResTm = do
                f <- mkFnTm
                x <- mkArgTm
                pure (tmApp f x)
            pure (mkResTm, resTy)
          else do
            -- Drop this arg for error-tolerance
            pure (mkFnTm, fnTy)

  applyHoles :: ApplyHoles -> Expr -> IO Tm -> Vy -> IO (IO Tm, Vy)
  applyHoles cfg@ApplyHoles{appendAuto, appendForall} exprRef mkTm ty = do
    ty <- forceTyWeak (eval cxt) ty
    case ty of
      VyArr Auto dom cod | appendAuto -> do
        -- Fn wants auto
        -- ... Produce an auto argument
        let
          mkResTm = do
            f <- mkTm
            solution <- tryResolveInstance cxt exprRef dom
            x <- case solution of
              Just sol -> do
                pure sol
              Nothing -> do
                -- Use a substitute for error-tolerance
                domDoc <- cxtRenderVy cxt dom
                markExprError cxt exprRef $ "No solution for auto: " <> domDoc
                pure TmError
            pure (tmApp f x)
        applyHoles cfg exprRef mkResTm cod
      VyForall kind cod | appendForall -> do
        -- Fn wants forall
        -- ... Generate a type hole
        ty <- cxtFreshTyMeta cxt kind MetaReason{expr = exprRef, desc = "type argument hole"}
        cod <- evalTyClosure (eval cxt) cod ty
        applyHoles cfg exprRef mkTm cod
      _ -> do
        -- Done
        pure (mkTm, ty)

-- * Elab fluent

elabFluent :: Cxt -> Expr -> NonEmpty Expr -> CheckTy -> IO (Tm, Vy)
elabFluent cxt self (headExpr :| tailExprs) check = do
  -- Process the list of fluent modifiers as though we are doing Polish
  -- notation.
  let
    fluents =
      fromRight impossible $
        P.matchList
          self
          (V.fromList tailExprs)
          (P.manyOf pModifier)

    app =
      foldl'
        (\acc modifier -> modifier cxt acc)
        (EApp'Tm (inferTm cxt headExpr))
        fluents
  elabApp cxt self app check
 where
  pModifier :: P.PList (Cxt -> EApp -> EApp)
  pModifier =
    asum
      [ (\fn _cxt app -> EApp'CallFn app fn)
          <$> (P.item (P.sym ".") *> P.item P.anyExpr)
      , (\key _cxt app -> EApp'GetDictKey app key)
          <$> (P.item (P.sym "./") *> P.item (P.capture P.anySym))
      , (\tyArg _cxt app -> EApp'ApplyTyArg app tyArg)
          <$> P.item (P.list (P.item (P.sym "type") *> P.item P.anyExpr))
      , (\tmArg _cxt app -> EApp'ApplyTmArg app tmArg (\check -> elabTm cxt tmArg check))
          <$> P.item P.anyExpr
      ]

-- * Elab utils

checkTm :: Cxt -> Expr -> Vy -> IO Tm
checkTm cxt self expTy = do
  fst <$> elabTm cxt self (CheckTy expTy)

inferTm :: Cxt -> Expr -> IO (Tm, Vy)
inferTm cxt self = do
  elabTm cxt self InferTy

forceCheckTyWeak :: Cxt -> CheckTy -> IO CheckTy
forceCheckTyWeak cxt check = do
  case check of
    CheckTy ty -> do
      ty <- forceTyWeak (eval cxt) ty
      pure $ CheckTy ty
    InferTy -> do
      pure InferTy

onCheckedTm :: Tm -> CheckTy -> IO (Tm, Vy)
onCheckedTm t check = do
  case check of
    CheckTy ty -> do
      pure (t, ty)
    InferTy -> do
      panic $ "checkedTm called with Infer"

onInferredTm :: Cxt -> Expr -> Tm -> Vy -> CheckTy -> IO (Tm, Vy)
onInferredTm cxt self x gotTy check = do
  case check of
    CheckTy expTy -> do
      tryUnifyTy cxt gotTy expTy >>= \case
        Left _ -> do
          gotTyDoc <- cxtRenderVy cxt =<< forceTy (eval cxt) gotTy
          expTyDoc <- cxtRenderVy cxt =<< forceTy (eval cxt) expTy
          makeAndMarkErrorElabTm cxt self check $
            "Has type: " <> show gotTyDoc <> "\nShould be: " <> expTyDoc
        Right () -> do
          pure (x, expTy)
    InferTy -> do
      pure (x, gotTy)

-- * Error handling

makeAndMarkErrorTm :: Cxt -> Expr -> PP.Doc Void -> IO Tm
makeAndMarkErrorTm cxt self msg = do
  Cxt.markExprError cxt self msg
  pure TmError

makeAndMarkErrorElabTm :: Cxt -> Expr -> CheckTy -> PP.Doc Void -> IO (Tm, Vy)
makeAndMarkErrorElabTm cxt self check msg = do
  t <- case check of
    CheckTy expTy -> do
      pure expTy
    InferTy -> do
      cxtFreshTyMeta cxt KStar (MetaReason self "type placeholder")
  x <- makeAndMarkErrorTm cxt self msg
  pure (x, t)
