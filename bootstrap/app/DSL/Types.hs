module DSL.Types (module DSL.Types, Name) where

import DSL.Dict qualified as Dict
import DSL.Name
import Data.Align (alignWith)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.These
import Data.Vector qualified as V
import Optics
import Prettyprinter qualified as PP
import Relude
import Utils.Misc (panic)

-- * Database ID

newtype UID = UID {unUID :: Int}
  deriving (Show, Eq, Generic)

instance PP.Pretty UID where
  pretty (UID n) = "UID:" <> PP.pretty n

instance Hashable UID

-- * Ix & Lvl

type Ix = Int
type Lvl = Int

-- * Spine

newtype Spine a = Spine (Seq a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

spineIsNil :: Spine a -> Bool
spineIsNil (Spine seq) = Seq.null seq

spineNil :: Spine a
spineNil = Spine Seq.empty

spineUncons :: Spine a -> Maybe (a, Spine a)
spineUncons (Spine seq) = case Seq.viewl seq of
  Seq.EmptyL -> Nothing
  x Seq.:< xs -> Just (x, Spine xs)

spineAppend :: Spine a -> a -> Spine a
spineAppend (Spine seq) x = Spine (seq Seq.|> x)

spineConcat :: Spine a -> Spine a -> Spine a
spineConcat (Spine xs) (Spine ys) = Spine (xs Seq.>< ys)

spineOne :: a -> Spine a
spineOne x = Spine (Seq.singleton x)

spineFromList :: [a] -> Spine a
spineFromList xs = Spine (Seq.fromList xs)

spineToList :: Spine a -> [a]
spineToList (Spine seq) = toList seq

spineLastArg :: Spine a -> Maybe a
spineLastArg (Spine (_ Seq.:|> x)) = Just x
spineLastArg (Spine _) = Nothing

{- | Example:

>>> splits "abc"
[("", "abc"), ("a", "bc"), ("ab", "c"), ("abc", "")]
-}
spineSplits :: Spine a -> Seq (Spine a, Spine a)
spineSplits (Spine seq) = Seq.zip (Spine <$> Seq.inits seq) (Spine <$> Seq.tails seq)

spineSplitAt :: Int -> Spine a -> (Spine a, Spine a)
spineSplitAt n (Spine seq) = let (a, b) = Seq.splitAt n seq in (Spine a, Spine b)

-- * Scope

data Scope a = Scope
  { scopeEntries :: Seq (Name, a)
  , scopeNameMap :: HashMap Name (Lvl, a)
  }
  deriving (Show)

scopeNil :: Scope a
scopeNil = Scope mempty mempty

scopeNullifyNameMap :: Scope a -> Scope a
scopeNullifyNameMap scope =
  Scope (scopeEntries scope) mempty

scopeNames :: Scope a -> Env Name
scopeNames scope = Env (fst <$> scopeEntries scope)

scopeEnv :: Scope a -> Env a
scopeEnv scope = Env (snd <$> scopeEntries scope)

scopeLvl :: Scope a -> Lvl
scopeLvl scope = Seq.length (scopeEntries scope)

scopePush :: Name -> a -> Scope a -> Scope a
scopePush name x scope =
  Scope
    ((name, x) Seq.<| scopeEntries scope)
    (HashMap.insert name (scopeLvl scope, x) (scopeNameMap scope))

scopeOne :: Name -> a -> Scope a
scopeOne name x = scopePush name x scopeNil

scopeResolve :: Name -> Scope a -> Maybe (Ix, a)
scopeResolve name scope = do
  (l, a) <- scopeNameMap scope HashMap.!? name
  pure (scopeLvl scope - l - 1, a)

-- | Returns a 'HashMap' that acts like the function 'scopeResolve'
scopeResolveMap :: Scope a -> HashMap Name (Ix, a)
scopeResolveMap scope =
  scopeNameMap scope <&> \(l, a) -> (scopeLvl scope - l - 1, a)

scopeAt :: Ix -> Scope a -> Maybe (Name, a)
scopeAt i scope = scopeEntries scope Seq.!? i

scopeAt' :: (HasCallStack) => Ix -> Scope a -> (Name, a)
scopeAt' i s = case scopeAt i s of
  Nothing -> panic $ "scopeIx: index " <> show i <> " out of bounds in scope with " <> show (scopeLvl s) <> " entries"
  Just a -> a

scopePushMany :: [(Name, a)] -> Scope a -> Scope a
scopePushMany [] scope = scope
scopePushMany ((name, a) : rest) scope = scopePushMany rest (scopePush name a scope)

scopeFromPushMany :: [(Name, a)] -> Scope a
scopeFromPushMany = flip scopePushMany scopeNil

scopeToPushMany :: Scope a -> [(Name, a)]
scopeToPushMany scope = reverse $ toList (scopeEntries scope)

scopeExtend :: Scope a -> Scope a -> Scope a
scopeExtend sub self = do
  let ofA (aLvl, a) = (aLvl + scopeLvl self, a)
  Scope
    (scopeEntries sub Seq.>< scopeEntries self)
    ( alignWith
        ( \entry -> do
            case entry of
              This a -> ofA a
              That b -> b
              These a _b -> ofA a
        )
        (scopeNameMap sub)
        (scopeNameMap self)
    )

-- * Env

newtype Env a = Env (Seq a)
  deriving (Show, Eq)

envNil :: Env a
envNil = Env mempty

envOne :: a -> Env a
envOne x = Env (Seq.singleton x)

envAt :: Ix -> Env a -> Maybe a
envAt i (Env as) = as Seq.!? i

envIx :: (HasCallStack) => Ix -> Env a -> a
envIx i env =
  fromMaybe
    (panic $ "envIx: index (" <> show i <> ") out of bounds (env lvl: " <> show (envLvl env) <> ")")
    (envAt i env)

envPush :: a -> Env a -> Env a
envPush x (Env as) = Env (x Seq.<| as)

envExtend :: Env a -> Env a -> Env a
envExtend (Env sub) (Env as) = Env (sub Seq.>< as)

envFromSp :: Spine a -> Env a
envFromSp (Spine as) = Env (Seq.reverse as)

envToSp :: Env a -> Spine a
envToSp (Env as) = Spine (Seq.reverse as)

envToList :: Env a -> [a]
envToList (Env as) = toList as

envFromList :: [a] -> Env a
envFromList xs = Env (Seq.fromList xs)

envLvl :: Env a -> Lvl
envLvl (Env as) = Seq.length as

{- | Build a 'Env' by pairs of (index, value). The index is only used for
sorting the input list so that the resulting 'Env' has the correct order.

If the input list has duplicate indices or is incomplete, the behavior is undefined.
-}
envFromMap :: [(Int, a)] -> Env a
envFromMap pairs = Env $ Seq.fromList $ map snd $ List.sortOn fst pairs

-- * Known type

data KnownType
  = KnownType'U8
  | KnownType'U16
  | KnownType'U32
  | KnownType'U64
  | KnownType'UInt
  | KnownType'I8
  | KnownType'I16
  | KnownType'I32
  | KnownType'I64
  | KnownType'Int
  | KnownType'F32
  | KnownType'F64
  | KnownType'String
  | KnownType'Bool
  | KnownType'Char
  | KnownType'Vec
  | KnownType'Bytes
  deriving (Show, Eq, Enum, Bounded, Generic)

knownTypeKind :: KnownType -> Kind
knownTypeKind = \case
  KnownType'U8 -> KStar
  KnownType'U16 -> KStar
  KnownType'U32 -> KStar
  KnownType'U64 -> KStar
  KnownType'UInt -> KStar
  KnownType'I8 -> KStar
  KnownType'I16 -> KStar
  KnownType'I32 -> KStar
  KnownType'I64 -> KStar
  KnownType'Int -> KStar
  KnownType'F32 -> KStar
  KnownType'F64 -> KStar
  KnownType'String -> KStar
  KnownType'Bool -> KStar
  KnownType'Char -> KStar
  KnownType'Bytes -> KStar
  KnownType'Vec -> KArr KStar KStar

knownTypeName :: KnownType -> Name
knownTypeName = \case
  KnownType'U8 -> "U8"
  KnownType'U16 -> "U16"
  KnownType'U32 -> "U32"
  KnownType'U64 -> "U64"
  KnownType'UInt -> "UInt"
  KnownType'I8 -> "I8"
  KnownType'I16 -> "I16"
  KnownType'I32 -> "I32"
  KnownType'I64 -> "I64"
  KnownType'Int -> "Int"
  KnownType'F32 -> "F32"
  KnownType'F64 -> "F64"
  KnownType'String -> "String"
  KnownType'Bool -> "Bool"
  KnownType'Char -> "Char"
  KnownType'Bytes -> "Bytes"
  KnownType'Vec -> "Vec"

asKnownType :: Name -> Maybe KnownType
asKnownType name = HashMap.lookup name mapping
 where
  mapping = HashMap.fromList $ universe @KnownType <&> \k -> (knownTypeName k, k)

-- * Kind

newtype KindMetaId = KindMetaId {unKindMetaId :: Int}
  deriving (Show, Eq)
  deriving newtype (Hashable)

data Kind = KStar | KArr Kind Kind | KMeta KindMetaId
  deriving (Show, Eq)

-- * Ty

type TyIx = Ix
type TyLvl = Lvl

newtype TyMetaId = TyMetaId {unTyMetaId :: Int}
  deriving (Show, Eq)
  deriving newtype (Hashable)

data Icit = Expl | Auto
  deriving (Show, Eq)

data Ty
  = TyVar TyIx
  | TyMeta TyMetaId
  | -- | @(a : A) -> B, where A : Type, a : A@
    TyArr Icit Ty Ty
  | -- | @(a : A) -> B, where A : Kind, a : Type@
    TyForall Kind Ty
  | -- | @\\(a : A) \=> B, where A : Kind, a : Type@
    TyLam Kind Ty
  | TyApp Ty Ty
  | TyTuple (V.Vector Ty)
  | TyDict (Dict.Dict Ty)
  | TyClass UID
  | TyDef UID
  | TyKnown KnownType
  deriving (Show, Eq)

-- * Vy

data VyHead
  = VyBound TyLvl
  | VyKnown KnownType
  | VyClass UID
  deriving (Show, Eq)

type TyEnv = Env Vy

data TyClosure = TyClosure TyEnv Ty
  deriving (Show, Eq)

data Vy
  = VyFlex TyMetaId (Spine Vy)
  | VyRigid VyHead (Spine Vy)
  | VyTuple (V.Vector Vy)
  | VyDict (Dict.Dict Vy)
  | VyArr Icit Vy Vy
  | VyForall Kind TyClosure
  | VyLam Kind TyClosure
  deriving (Show, Eq)

-- * Literal

data Lit
  = LString Text
  | LI32 Int32
  | LBool Bool
  | LChar Char
  | LBytes ByteString
  deriving (Show, Eq)

prettyLit :: Lit -> PP.Doc ann
prettyLit = \case
  LString s -> PP.viaShow s
  LBool b -> if b then "true" else "false"
  LChar c -> PP.viaShow c
  LBytes bs -> PP.viaShow bs
  LI32 i32 -> PP.pretty i32

-- * Tm

newtype IsTail = IsTail Bool
  deriving (Show, Eq)

type TmIx = Ix
type TmLvl = Lvl

newtype MemberID = MemberID {unMemberID :: Int}
  deriving (Show, Eq)

data Tm
  = TmVar TmIx
  | TmDef UID
  | TmLam Tm
  | TmApp IsTail Tm Tm
  | -- | Late-binding recursive let
    TmLet [Tm] Tm
  | -- Access
    TmGetTupleIndex
      -- | Target
      Tm
      -- | Field index
      Int
  | TmSetTupleIndex
      -- | Target
      Tm
      -- | Field index
      Int
      -- | New value
      Tm
  | TmGetDictKey Tm Name
  | -- Construction
    TmLit Lit
  | TmClass UID MemberID Tm
  | TmTuple [Tm]
  | TmVec [Tm]
  | TmDict
      -- | Create a dictionary from the given fields. The field order is
      -- PRESERVED to guarantee code execution order.
      [(Name, Tm)]
  | -- Control flow
    TmIf Tm Tm Tm
  | TmMatch MatchTree
  | TmError
  deriving (Show)

-- * Pattern matching

{- | Number of pattern variables between the root and to a particular point in
the tree.
-}
type PatLvl = Int

newtype MatchClauseID = MatchClauseID {unMatchClauseID :: Int}
  deriving (Show, Eq, Enum, Ord, Generic)
  deriving newtype (Hashable)

data MatchLeaf = MatchLeaf
  { targetClauseID :: MatchClauseID
  -- ^ The ID of the clause this leaf corresponds to.
  , patEnv :: Env PatLvl
  -- ^ The pattern bound variables to apply to the clause with.
  , onGuardFail :: Maybe MatchNode
  -- ^ If 'targetClauseID' has a guard, this is the 'MatchNode' to jump to if
  -- the guard fails. MUST be 'Just' if and only if the clause has a guard.
  }
  deriving (Show)

data MatchBranchClass = MatchBranchClass
  { memberID :: MemberID
  , next :: MatchNode
  }
  deriving (Show)

data MatchBranchTuple = MatchBranchTuple
  { numFields :: PatLvl
  , next :: MatchNode
  }
  deriving (Show)

data MatchBranchBool = MatchBranchBool
  { value :: Bool
  , next :: MatchNode
  }
  deriving (Show)

data MatchNode
  = -- | A leaf that corresponds to the "unhandled" case. Used for exhaustiveness checking.
    MatchNode'LeafUnhandled
  | MatchNode'Leaf MatchLeaf
  | MatchNode'BranchClass PatLvl [MatchBranchClass]
  | MatchNode'BranchTuple PatLvl MatchBranchTuple
  | MatchNode'BranchBool PatLvl [MatchBranchBool]
  deriving (Show)

data MatchClause = MatchClause
  { id :: MatchClauseID
  , patEnvLvl :: PatLvl
  -- ^ The number of pattern variables this clause's 'term'. Used by compilers
  -- for codegen purposes.
  --
  -- 'MatchLeaf's that target this clause MUST have the same number of 'args' as
  -- this.
  , bodyTm :: Tm
  -- ^ The RHS of the clause.
  , guardTm :: Maybe Tm
  -- ^ A guard is a boolean expression that must hold for the clause to match.
  -- If the guard fails, the corresponding 'MatchLeaf' will jump to
  -- 'onGuardFail'.
  }
  deriving (Show)

-- | A match decision tree. Leafs of the tree correspond to match clauses.
data MatchTree = MatchTree
  { scrutinee :: Tm
  -- ^ The root scrutinee. Can be referred to by PatLvl = 0.
  , root :: MatchNode
  , clauses :: [MatchClause]
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''MatchClause
makeFieldLabelsNoPrefix ''MatchTree
