module DSL.Utils where

import DSL.Name
import DSL.Types
import Data.Foldable
import Data.HashSet qualified as HashSet
import Relude

-- * Kind utils

isKindArityKnown :: Kind -> Bool
isKindArityKnown = \case
  KArr _ cod -> isKindArityKnown cod
  KMeta{} -> False
  KStar -> True

-- | For example, @dropKindDoms 2 ((* -> *) -> (* -> *) -> * -> *) = (* -> *)@
dropKindDoms :: (HasCallStack) => Int -> Kind -> Kind
dropKindDoms n star = case star of
  KArr _ rest | n > 0 -> dropKindDoms (n - 1) rest
  _ | n == 0 -> star
  _ -> error "dropKindDoms: got less params than expected"

-- * Ty utils

tyUnit :: Ty
tyUnit = TyTuple mempty

tyLam :: Kind -> Ty -> Ty
tyLam = TyLam

tyLams :: Env Kind -> Ty -> Ty
tyLams env = go (envToList env)
 where
  go :: [Kind] -> Ty -> Ty
  go [] cod = cod
  go (k : ks) cod = go ks (TyLam k cod)

{- | Like 'vyBoundSp', but for 'Ty'.

@tyVarSp 3@ = @[TyVar 2, TyVar 1, TyVar 0]@
-}
tyVarSp :: TyLvl -> Spine Ty
tyVarSp count = spineFromList $ reverse [TyVar i | i <- [0 .. count - 1]]

tyUnfoldApp :: Ty -> (Ty, [Ty])
tyUnfoldApp f = go f []
 where
  go :: Ty -> [Ty] -> (Ty, [Ty])
  go (TyApp f x) xs = go f (x : xs)
  go f xs = (f, xs)

-- | The opposite of 'tyUnfoldApp'
tyFoldApp :: Ty -> Spine Ty -> Ty
tyFoldApp = foldl' TyApp

tyUnfoldArr' :: Ty -> ([(Icit, Ty)], Ty)
tyUnfoldArr' = \case
  TyArr icit dom cod -> do
    let (doms, cod') = tyUnfoldArr' cod
    ((icit, dom) : doms, cod')
  cod -> do
    ([], cod)

tyUnfoldArr :: Ty -> ([Ty], Ty)
tyUnfoldArr = first (fmap snd) . tyUnfoldArr'

-- | The opposite of 'tyUnfoldArr'
tyFoldArr :: [Ty] -> Ty -> Ty
tyFoldArr doms cod = foldr (TyArr Expl) cod doms

tyArr :: Ty -> Ty -> Ty
tyArr = TyArr Expl

-- * Vy constructors

-- | @vyBoundSp 2 3 = [vyBound 2, vyBound 3, vyBound 4]@
vyBoundSp :: TyLvl -> TyLvl -> Spine Vy
vyBoundSp lvl count = spineFromList [vyBound (lvl + l) | l <- [0 .. count - 1]]

-- | @vyBoundEnv 3@ = [vyBound 2, vyBound 1, vyBound 0]
vyBoundEnv :: TyLvl -> TyEnv
vyBoundEnv lvl = envFromSp (vyBoundSp 0 lvl)

vyBound :: TyLvl -> Vy
vyBound i = VyRigid (VyBound i) spineNil

vyMeta :: TyMetaId -> Vy
vyMeta m = VyFlex m spineNil

vyClass :: UID -> Vy
vyClass u = VyRigid (VyClass u) spineNil

vyKnown :: KnownType -> Vy
vyKnown k = VyRigid (VyKnown k) spineNil

vyBool :: Vy
vyBool = vyKnown KnownType'Bool

vyUnit :: Vy
vyUnit = VyTuple mempty

-- * Kind utils

kindFoldMap :: [Kind] -> Kind -> Kind
kindFoldMap argTys cod = foldr KArr cod argTys

kindUnfoldMap :: Kind -> ([Kind], Kind)
kindUnfoldMap = \case
  KArr dom codIn -> do
    let (doms, cod) = kindUnfoldMap codIn
    (dom : doms, cod)
  cod -> do
    ([], cod)

scanKindMetaIds :: Kind -> HashSet KindMetaId
scanKindMetaIds = \case
  KStar -> mempty
  KArr k1 k2 -> scanKindMetaIds k1 <> scanKindMetaIds k2
  KMeta m -> HashSet.singleton m

-- * Ty scope

type TyScope = Scope Kind

tyScopeFoldAsKArrs :: TyScope -> Kind -> Kind
tyScopeFoldAsKArrs scope = go (toList (scopeEntries scope))
 where
  go :: [(Name, Kind)] -> Kind -> Kind
  go [] cod = cod
  go ((_n, k) : rest) cod = go rest (KArr k cod)

tyScopeFoldAsForalls :: TyScope -> Ty -> Ty
tyScopeFoldAsForalls scope = go (toList (scopeEntries scope))
 where
  go :: [(Name, Kind)] -> Ty -> Ty
  go [] cod = cod
  go ((_n, k) : rest) cod = go rest (TyForall k cod)

-- * Tm scope

type TmScope = Scope Vy

-- * Tm utils

tmApp :: Tm -> Tm -> Tm
tmApp = TmApp (IsTail False)

tmFoldApp :: Tm -> [Tm] -> Tm
tmFoldApp = foldl' tmApp

tmVarSp :: TmLvl -> [Tm]
tmVarSp count = reverse [TmVar i | i <- [0 .. count - 1]]

tmFoldLams :: TmLvl -> Tm -> Tm
tmFoldLams lvl e
  | lvl < 0 = error "tmFoldLams: negative lvl"
  | lvl == 0 = e
  | otherwise = tmFoldLams (lvl - 1) (TmLam e)
