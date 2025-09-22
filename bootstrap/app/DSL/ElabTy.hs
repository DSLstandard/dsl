module DSL.ElabTy (checkTy, inferTy, CheckKind (..)) where

import DSL.Cxt
import DSL.Cxt qualified as Cxt
import DSL.Database
import DSL.Dict qualified as Dict
import DSL.ElabKind
import DSL.EvalKind
import DSL.MetaStore
import DSL.PatUtils
import DSL.PrettyKind
import DSL.QuoteTy
import DSL.Types
import DSL.UnifyKind
import DSL.UnifyUtils
import DSL.Utils
import DSL.Expr
import DSL.Pat qualified as P
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable
import Data.Vector qualified as V
import Prettyprinter qualified as PP
import Relude
import Utils.Misc
import Witherable
import DSL.CxtUtils

data CheckKind = CheckKind Kind | InferKind

elabTy :: Cxt -> Expr -> CheckKind -> IO (Ty, Kind)
elabTy cxt self check = do
  matchOrReport cxt self pat >>= \case
    Nothing -> do
      makeAndMarkErrorElabTy cxt self check "Bad type expression"
    Just io -> do
      io
 where
  pat =
    mconcat
      [ patTyTuple cxt check
      , patTyArrow cxt check
      , patTyDict cxt check
      , patTyLam cxt check
      , P.sym "," $> do
          onInferredTy cxt self (TyTuple mempty) KStar check
      , P.label "<variable>" P.anySym <&> \name -> do
          elabTyVar cxt self name check
      , P.list ((,) <$> P.item P.anyExpr <*> P.rest) <&> \(f, xs) -> do
          elabTyApp cxt self f xs check
      ]

-- * Elab [\ ...]

patTyLam :: Cxt -> CheckKind -> P.PExpr (IO (Ty, Kind))
patTyLam cxt inCheck = do
  P.block_ "\\" $
    ( \entries -> do
        let inDoms = NonEmpty.init entries
        let cod = NonEmpty.last entries

        let
          go :: [Expr] -> Cxt -> CheckKind -> IO (Ty, Kind)
          go [] cxt check = do
            elabTy cxt cod check
          go (t : ts) cxt check = do
            parse <- matchOrReport cxt t (patNameOnlyParam <> patParam)
            case parse of
              Nothing -> do
                -- Skip for error-tolerance.
                go ts cxt check
              Just parse -> do
                parse cxt check (go ts)

        go inDoms cxt inCheck
    )
      <$> P.someItem (P.label "<entry>" P.anyExpr)
 where
  elabParam :: (Expr, Name, Maybe Expr) -> Cxt -> CheckKind -> (Cxt -> CheckKind -> IO (Ty, Kind)) -> IO (Ty, Kind)
  elabParam (self, name, domK'expr) cxt check doRest = do
    check <- forceCheckKindWeak cxt check
    case check of
      CheckKind (KArr domK codK) -> do
        -- Check against 'check'
        whenJust domK'expr \domK'expr -> do
          got'domK <- elabKind cxt domK'expr
          void $ unifyKindOrReport cxt domK'expr got'domK domK
        let cxt' = cxtExtendTy (scopeOne name domK) cxt
        (rest'ty, _) <- doRest cxt' (CheckKind codK)
        onCheckedTy (TyLam domK rest'ty) check
      _ -> do
        -- Infer for 'check'
        domK <- case domK'expr of
          Nothing -> do
            cxtFreshKindMeta cxt (MetaReason self "kind placeholder")
          Just domK'expr -> do
            elabKind cxt domK'expr
        let cxt' = cxtExtendTy (scopeOne name domK) cxt
        (rest'ty, rest'kind) <- doRest cxt' InferKind
        onInferredTy cxt self (TyLam domK rest'ty) (KArr domK rest'kind) check

  patNameOnlyParam :: P.PExpr (Cxt -> CheckKind -> (Cxt -> CheckKind -> IO (Ty, Kind)) -> IO (Ty, Kind))
  patNameOnlyParam =
    P.label "<name>" (P.capture P.anySym) <&> \(self, name) -> do
      elabParam (self, name, Nothing)

  patParam :: P.PExpr (Cxt -> CheckKind -> (Cxt -> CheckKind -> IO (Ty, Kind)) -> IO (Ty, Kind))
  patParam =
    P.captures . P.list $
      ( \name domK'expr self -> elabParam (self, name, Just domK'expr)
      )
        <$> P.item (P.label "<name>" P.anySym)
        <*> P.item (P.label "<kind>" P.anyExpr)

-- * Elab [-> ...]

patTyArrow :: Cxt -> CheckKind -> P.PExpr (IO (Ty, Kind))
patTyArrow cxt check =
  P.block "->" $
    ( \entries P.BlockInfo{self} -> do
        elab self entries check
    )
      <$> P.someItem (P.label "<domain>" P.anyExpr)
 where
  elab :: Expr -> NonEmpty Expr -> CheckKind -> IO (Ty, Kind)
  elab self entries check = do
    let
      inDoms = NonEmpty.init entries
      cod = NonEmpty.last entries

      go :: Cxt -> [Expr] -> IO Ty
      go cxt [] = do
        checkTy cxt cod KStar
      go cxt (dom : doms) = do
        let run = matchOr dom (patAutoArr <> patForall) (elabExpl dom)
        run cxt (\cxt' buildType -> buildType <$> go cxt' doms)

    ty <- go cxt inDoms
    onInferredTy cxt self ty KStar check

  elabExpl :: Expr -> Cxt -> (Cxt -> (Ty -> Ty) -> IO Ty) -> IO Ty
  elabExpl typeExpr cxt checkRest = do
    dom <- checkTy cxt typeExpr KStar
    checkRest cxt (TyArr Expl dom)

  patAutoArr :: P.PExpr (Cxt -> (Cxt -> (Ty -> Ty) -> IO Ty) -> IO Ty)
  patAutoArr =
    P.block_ "auto" $
      ( \domExpr cxt checkRest -> do
          dom <- checkTy cxt domExpr KStar
          checkRest cxt (TyArr Auto dom)
      )
        <$> P.item (P.label "<type>" P.anyExpr)

  patForall :: P.PExpr (Cxt -> (Cxt -> (Ty -> Ty) -> IO Ty) -> IO Ty)
  patForall =
    P.block "type" $
      ( \name kindExpr P.BlockInfo{self} cxt checkRest -> do
          kind <- case kindExpr of
            Nothing -> do
              cxtFreshKindMeta cxt MetaReason{expr = self, desc = "kind placeholder"}
            Just kindExpr -> do
              elabKind cxt kindExpr
          checkRest (cxtExtendTy (scopeOne name kind) cxt) (TyForall kind)
      )
        <$> P.item (P.label "<name>" P.anySym)
        <*> P.optionalItem (P.label "<kind>" P.anyExpr)

-- * Elab app

elabTyApp :: Cxt -> Expr -> Expr -> [Expr] -> CheckKind -> IO (Ty, Kind)
elabTyApp cxt self inF inArgs check = do
  (f, fKind) <- inferTy cxt inF
  go f fKind inArgs
 where
  go :: Ty -> Kind -> [Expr] -> IO (Ty, Kind)
  go f fKind [] = do
    onInferredTy cxt self f fKind check
  go f fKind (x : xs) = do
    fKind <- forceKindWeak (eval cxt) fKind
    case fKind of
      KArr dom cod -> do
        x <- checkTy cxt x dom
        go (TyApp f x) cod xs
      _ -> do
        (xTy, xKind) <- inferTy cxt x
        yKind <- cxtFreshKindMeta cxt (MetaReason x "return kind placeholder")
        tryUnifyKind cxt fKind (KArr xKind yKind) >>= \case
          Left _ -> do
            makeAndMarkErrorElabTy cxt self check $
              "Cannot apply type of kind: " <> show fKind <> "\nExpected a function kind"
          Right () -> do
            go (TyApp f xTy) yKind xs

-- * Elab dict

patTyDict :: Cxt -> CheckKind -> P.PExpr (IO (Ty, Kind))
patTyDict cxt check = do
  P.block "Dict" $
    ( \entries P.BlockInfo{self} -> do
        entries <- forMaybe entries \entryExpr -> do
          matchOrReport cxt entryExpr patEntry

        entries <- for entries \(key, valueExpr) -> do
          value <- checkTy cxt valueExpr KStar
          pure (key, value)

        let dict = Dict.fromList entries
        onInferredTy cxt self (TyDict dict) KStar check
    )
      <$> P.manyItem (P.label "<entry>" P.anyExpr)
 where
  patEntry :: P.PExpr (Name, Expr)
  patEntry =
    P.list $
      (,)
        <$> P.item (P.label "<key>" P.anySym)
        <*> P.item (P.label "<value>" P.anyExpr)

-- * Elab tuple

patTyTuple :: Cxt -> CheckKind -> P.PExpr (IO (Ty, Kind))
patTyTuple cxt check =
  P.block ";" $
    ( \elems P.BlockInfo{self} -> do
        elems <- for elems \elem -> do
          checkTy cxt elem KStar
        onInferredTy cxt self (TyTuple (V.fromList elems)) KStar check
    )
      <$> P.manyItem (P.label "<type>" P.anyExpr)

-- * Elab var

elabTyVar :: Cxt -> Expr -> Name -> CheckKind -> IO (Ty, Kind)
elabTyVar cxt nameExpr name check = do
  asumL
    [ do
        -- Try resolving as a local type variable
        (i, kind) <- hoistMaybe $ scopeResolve name cxt.tyScope
        liftIO do
          onInferredTy cxt nameExpr (TyVar i) kind check
    , do
        -- Try resolving as a class/tydef reference
        uid <- MaybeT $ lookupNamespace cxt name
        liftIO do
          entry <- queryEntry cxt.db uid
          case entry of
            XEntry'TyDef tydef -> do
              onInferredTy cxt nameExpr (TyDef uid) tydef.kind check
            XEntry'Class xclass -> do
              onInferredTy cxt nameExpr (TyClass uid) (tyScopeFoldAsKArrs xclass.generics KStar) check
            _ -> do
              makeAndMarkErrorElabTy cxt nameExpr check ("Type variable '" <> PP.pretty name <> "' is not referencing a struct/enum/tydef")
    , do
        -- Try resolving as a known type
        k <- hoistMaybe $ asKnownType name
        lift do
          onInferredTy cxt nameExpr (TyKnown k) (knownTypeKind k) check
    ]
    ( do
        makeAndMarkErrorElabTy cxt nameExpr check ("Unknown type variable: '" <> PP.pretty name <> "'")
    )

-- * Elab utils

checkTy :: Cxt -> Expr -> Kind -> IO Ty
checkTy cxt self expKind = do
  fst <$> elabTy cxt self (CheckKind expKind)

inferTy :: Cxt -> Expr -> IO (Ty, Kind)
inferTy cxt self = do
  elabTy cxt self InferKind

forceCheckKindWeak :: Cxt -> CheckKind -> IO CheckKind
forceCheckKindWeak cxt check = do
  case check of
    CheckKind ty -> do
      ty <- forceKindWeak (eval cxt) ty
      pure $ CheckKind ty
    InferKind -> do
      pure InferKind

onCheckedTy :: Ty -> CheckKind -> IO (Ty, Kind)
onCheckedTy t check = do
  case check of
    InferKind -> do
      panic $ "checkedTy called with Infer"
    CheckKind kind -> do
      pure (t, kind)

onInferredTy :: Cxt -> Expr -> Ty -> Kind -> CheckKind -> IO (Ty, Kind)
onInferredTy cxt self t gotKind check = do
  case check of
    InferKind -> do
      pure (t, gotKind)
    CheckKind expKind -> do
      tryUnifyKind cxt gotKind expKind >>= \case
        Left _ -> do
          makeAndMarkErrorElabTy cxt self check $
            "Has kind: " <> prettyKind gotKind <> "\nShould be: " <> prettyKind expKind
        Right () -> do
          pure (t, expKind)

-- * Error handling

makeAndMarkErrorTy :: Cxt -> Expr -> Kind -> PP.Doc Void -> IO Vy
makeAndMarkErrorTy cxt self kind msg = do
  Cxt.markExprError cxt self msg
  cxtFreshTyMeta cxt kind (MetaReason self msg)

makeAndMarkErrorElabTy :: Cxt -> Expr -> CheckKind -> PP.Doc Void -> IO (Ty, Kind)
makeAndMarkErrorElabTy cxt self check msg = do
  (t, kind) <- case check of
    CheckKind expKind -> do
      t <- makeAndMarkErrorTy cxt self expKind (msg <> "\nExpected kind: " <> prettyKind expKind)
      pure (t, expKind)
    InferKind -> do
      kind <- cxtFreshKindMeta cxt (MetaReason self "kind placeholder")
      t <- makeAndMarkErrorTy cxt self kind (msg <> "\nPlaceholder kind: " <> prettyKind kind)
      pure (t, kind)

  t <- quoteTy (eval cxt) (scopeLvl cxt.tyScope) t
  pure (t, kind)
