module DSL.UnifyTy where

import Colog qualified
import Control.Exception qualified as Exception
import DSL.Cxt
import DSL.CxtUtils
import DSL.Database
import DSL.Dict qualified as Dict
import DSL.EvalKind
import DSL.EvalTy
import DSL.MetaStore
import DSL.Types
import DSL.UnifyKind
import DSL.Utils
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Sequence qualified as Seq
import Data.Traversable
import Data.Tuple qualified as Tuple
import Relude
import Utils.Misc

newtype UnifyTyError = UnifyTyError CallStack
  deriving (Show)

instance Exception UnifyTyError

tryUnifyTy :: Cxt -> Vy -> Vy -> IO (Either UnifyTyError ())
tryUnifyTy cxt lhs rhs = do
  let env = scopeEnv cxt.tyScope

  when cxt.debugging do
    lhsDoc <- cxtRenderVy cxt lhs
    rhsDoc <- cxtRenderVy cxt rhs

    cxt.debugOut Colog.<& "[tryUnifyTy] "
      <> show lhsDoc
      <> " ~ "
      <> show rhsDoc

  Exception.try @UnifyTyError (unifyTyImpl cxt env lhs rhs)


unifyTyImpl :: (HasCallStack) => Cxt -> Env Kind -> Vy -> Vy -> IO ()
unifyTyImpl cxt = goTy
 where
  -- NOTE: Just 'TyLvl' is not enough. We need the whole 'TyScope' because
  -- attemptImitateRigid uses kinds.
  goTy :: Env Kind -> Vy -> Vy -> IO ()
  goTy env lhs rhs = do
    lhs <- forceTyWeak (eval cxt) lhs
    rhs <- forceTyWeak (eval cxt) rhs

    strongPairRef <- newIORef Nothing
    let
      getWeakPair :: (MonadIO m) => m (Vy, Vy)
      getWeakPair = pure (lhs, rhs)

      getStrongPair :: (MonadIO m) => m (Vy, Vy)
      getStrongPair = liftIO do
        readIORef strongPairRef >>= \case
          Just strongPair -> do
            pure strongPair
          Nothing -> do
            (v1, v2) <- getWeakPair
            v1 <- forceTy (eval cxt) v1
            v2 <- forceTy (eval cxt) v2
            writeIORef strongPairRef (Just (v1, v2))
            pure (v1, v2)

    -- NOTE: we cannot use case because guard functions cannot run IO.
    asumL
      [ do
          -- Trivial rigid-rigid
          (VyRigid h1 sp1, VyRigid h2 sp2) <- getWeakPair

          liftIO do
            unless (h1 == h2) failUnifyTy
            unless (length sp1 == length sp2) failUnifyTy
            zipWithM_ (goTy env) (toList sp1) (toList sp2)
      , do
          -- Trivial tuple-tuple
          (VyTuple xs, VyTuple ys) <- getWeakPair

          liftIO do
            unless (length xs == length ys) failUnifyTy
            zipWithM_ (goTy env) (toList xs) (toList ys)
      , do
          -- Trivial dict-dict
          (VyDict d1, VyDict d2) <- getWeakPair

          liftIO do
            unless (Dict.isAligned d1 d2) failUnifyTy
            for_ (Dict.zip d1 d2) $ \(a, b) -> do
              goTy env a b
      , do
          -- Trivial arr-arr
          (VyArr icit1 dom1 cod1, VyArr icit2 dom2 cod2) <- getWeakPair

          liftIO do
            unless (icit1 == icit2) failUnifyTy
            goTy env dom1 dom2
            goTy env cod1 cod2
      , do
          -- Trivial lam-lam
          (VyLam kind1 c1, VyLam kind2 c2) <- getWeakPair

          liftIO do
            result <- tryUnifyKind cxt kind1 kind2
            when (isLeft result) failUnifyTy

            let arg = vyBound (envLvl env)
            c1 <- evalTyClosure (eval cxt) c1 arg
            c2 <- evalTyClosure (eval cxt) c2 arg
            goTy (envPush kind1 env) c1 c2
      , do
          -- Eta-expansion
          pair <- getWeakPair
          attemptBothWays pair \(lhs, rhs) -> do
            VyLam kind c <- pure lhs
            liftIO do
              let arg = vyBound (envLvl env)
              c <- evalTyClosure (eval cxt) c arg
              rhs <- vyApp (eval cxt) rhs arg
              goTy (envPush kind env) c rhs
      , do
          -- Trivial forall-forall
          (VyForall kind1 cod1, VyForall kind2 cod2) <- getWeakPair

          liftIO do
            result <- tryUnifyKind cxt kind1 kind2
            when (isLeft result) failUnifyTy

            let arg = vyBound (envLvl env)
            cod1 <- evalTyClosure (eval cxt) cod1 arg
            cod2 <- evalTyClosure (eval cxt) cod2 arg
            goTy (envPush kind1 env) cod1 cod2
      , do
          -- Flex-flex with the same meta head
          (VyFlex m1 sp1, VyFlex m2 sp2) <- getWeakPair
          guard $ m1 == m2

          liftIO do
            -- TODO: Try pruning and intersection first, then the following.
            unless (length sp1 == length sp2) failUnifyTy
            zipWithM_ (goTy env) (toList sp1) (toList sp2)
      , do
          -- Try pattern unification
          pair <- getStrongPair
          attemptBothWays pair (attemptMiller env)
      , do
          -- NOTE: From this point on, we will resort to using heuristics.
          pair <- getStrongPair
          attemptBothWays pair (attemptImitateRigid env)
      ]
      ( do
          failUnifyTy
      )

  attemptBothWays :: (MonadPlus m) => (Vy, Vy) -> ((Vy, Vy) -> m r) -> m r
  attemptBothWays pair f = do
    f pair <|> f (Tuple.swap pair)

  failUnifyTy :: (HasCallStack, MonadIO m) => m a
  failUnifyTy = liftIO $ Exception.throwIO (UnifyTyError callStack)

  attemptMiller :: Env Kind -> (Vy, Vy) -> MaybeT IO ()
  attemptMiller env (v1, v2) = do
    (VyFlex m sp, rhs) <- pure (v1, v2)
    Right pren <- lift $ tryInvert cxt env sp
    liftIO do
      rename cxt m pren rhs >>= \case
        Left{} -> do
          failUnifyTy
        Right sol -> do
          sol <- evalTy (eval cxt) envNil (tyLams pren.dom sol)
          cxtSetTyMetaSolution cxt m sol

  -- Imitate a rigid head with a flex meta, and see if it unifies with
  -- the other side.
  attemptImitateRigid :: Env Kind -> (Vy, Vy) -> MaybeT IO ()
  attemptImitateRigid env (v1, v2) = do
    {-
      Problems to consider:
        Case 1:
          __then__ : (type M : * -> *) -> (type A : *) -> (type B -> *) -> M A -> (A -> M B) -> M B
          __then__(?1, ?2, ?3, Id 123, \(x) => Id x)
          ... We should unify: ?1 ?3 = Id 123

        Case 2:
          s <- getState()
          - has type:     State ?322 ?322
          - should match: ?319       ?320

          ... We should unify: ?319 = State ?322

        Case 3:
          has type:     ?882 V A B (?884 V A B)
          should match: Yield V    B

          ... We should unify: ?882 V A B = Yield V

        Case 4:
          `forListM_(i + 1, list, body)`

          has type: ?910 T M ()
          should match: M ()

          ... We should unify: ?910 T M = M

        In general:
          has type:     ?m i1 i2 i3 i4 ...
          should match: (C x1 x2 x3 x4 ...) : *
    -}

    (VyRigid head headSp, VyFlex m mSp) <- pure (v1, v2)

    headKind <- case head of
      VyBound l -> do
        let i = envLvl env - l - 1
        pure $ envIx i env
      VyKnown known -> do
        pure $ knownTypeKind known
      VyClass uid -> do
        cls <- liftIO $ queryClass cxt.db uid
        pure $ tyScopeFoldAsKArrs cls.generics KStar

    headKind <- liftIO $ forceKind (eval cxt) headKind
    guard $ isKindArityKnown headKind

    meta <- liftIO $ getTyMeta cxt.metas m
    mKind <- liftIO $ forceKind (eval cxt) meta.kind
    guard $ isKindArityKnown mKind

    Just (mSp0, mSp1) <-
      liftIO . firstJustM $
        Seq.reverse (spineSplits mSp) <&> \(sp0, sp1) -> do
          yes <- canInvert cxt env sp0
          pure $
            if yes
              then Just (sp0, sp1)
              else Nothing

    let headSp0Len = length headSp - length mSp1
    guard $ headSp0Len >= 0

    liftIO do
      let (headSp0, _headSp1) = spineSplitAt headSp0Len headSp

      result <-
        tryUnifyKind
          cxt
          (dropKindDoms (length headSp0) headKind)
          (dropKindDoms (length mSp0) mKind)
      when (isLeft result) do
        failUnifyTy

      -- Imitate then retry the original unification should yield new results.
      goTy env (VyRigid head headSp0) (VyFlex m mSp0)
      goTy env (VyRigid head headSp) (VyFlex m mSp)

-- * Inversion for Miller's algorithm

data PartialRenaming = PartialRenaming
  { dom :: Env Kind
  , cod :: TyLvl
  , ren :: IntMap TyLvl
  -- ^ Codomain tylvl -> Domain tylvl
  }
  deriving (Show)

liftPRen :: Kind -> PartialRenaming -> PartialRenaming
liftPRen kind (PartialRenaming dom cod ren) =
  PartialRenaming (envPush kind dom) (cod + 1) (IntMap.insert cod (envLvl dom) ren)

data InvertFail
  = -- | Violates one or more of the following:
    --
    -- If we are unifying: m sp ~ rhs, and we are inverting 'm sp'
    --
    --   1. 'sp' has duplicate bounds (e.g. m x x is not invertible)
    --
    --   2. 'sp' has items that are obviously not bounds (unless the item is an
    --      unsolved meta, since we might figure out later that it is in fact a
    --      'VyBound').
    --
    --   3. 'rhs' refers the 'm' itself.
    --
    -- However, if 'sp' contains unsolved metas AND we cannot be sure about the
    -- above conditions, we will report 'InvertFail'Blocked' instead.
    InvertFail'MustFail
  | -- | See comment of 'InvertFail'MustFail'
    InvertFail'Blocked
  deriving (Show)

instance Exception InvertFail

tryInvert ::
  Cxt ->
  Env Kind ->
  Spine Vy ->
  IO (Either InvertFail PartialRenaming)
tryInvert cxt codEnv sp = do
  -- FIXME: The code here has too many unnecessary double-checking because it
  -- has been updated too many times, may fix later.
  Exception.try @InvertFail go
 where
  mustFail :: IO a
  mustFail = Exception.throwIO InvertFail'MustFail

  blocked :: IO a
  blocked = Exception.throwIO InvertFail'Blocked

  go :: IO PartialRenaming
  go = do
    seenBoundsRef <- newIORef IntSet.empty
    foundUnsolvedMetasRef <- newIORef False

    sp <- traverse (forceTyWeak (eval cxt)) sp

    for_ sp \item -> do
      case item of
        VyRigid (VyBound l) sp | spineIsNil sp -> do
          isDuplicate <- IntSet.member l <$> readIORef seenBoundsRef
          when isDuplicate mustFail
          modifyIORef' seenBoundsRef (IntSet.insert l)
        VyFlex{} -> do
          -- We continue checking anyway. We might find a definite reason for
          -- failure later.
          writeIORef foundUnsolvedMetasRef True
        _ -> do
          mustFail

    hasUnsolvedMetas <- readIORef foundUnsolvedMetasRef
    when hasUnsolvedMetas blocked

    -- Revisit again but this time we construct the 'dom'
    let lvl = envLvl codEnv
    dom <-
      envFromSp <$> for sp \item -> do
        case item of
          VyRigid (VyBound l) sp | spineIsNil sp -> do
            let i = lvl - l - 1
            let kind = envIx i codEnv
            pure kind
          _ -> do
            impossible

    mapper <- doInvert (spineToList sp) 0 IntMap.empty
    pure PartialRenaming{dom = dom, cod = lvl, ren = mapper}

  doInvert :: [Vy] -> TyLvl -> IntMap TyLvl -> IO (IntMap TyLvl)
  doInvert [] _dom mapper = do
    pure mapper
  doInvert (x : xs) dom mapper = do
    x <- forceTyWeak (eval cxt) x
    case x of
      VyRigid (VyBound l) sp | spineIsNil sp && IntMap.notMember l mapper -> do
        doInvert xs (dom + 1) (IntMap.insert l dom mapper)
      _ -> do
        impossible

canInvert :: Cxt -> Env Kind -> Spine Vy -> IO Bool
canInvert c codEnv sp = isRight <$> tryInvert c codEnv sp

-- * Renaming for Miller's algorithm

data RenameFail
  = RenameFail'MustFail
  deriving (Show)

instance Exception RenameFail

-- | Rename a type. Returns 'Nothing' if it is impossible.
rename :: Cxt -> TyMetaId -> PartialRenaming -> Vy -> IO (Either RenameFail Ty)
rename cxt m inPren inTy = do
  Exception.try @RenameFail (go inPren inTy)
 where
  mustFail :: IO a
  mustFail = Exception.throwIO RenameFail'MustFail

  go :: PartialRenaming -> Vy -> IO Ty
  go pren input = do
    input <- forceTyWeak (eval cxt) input
    case input of
      VyLam kind c -> do
        t <- evalTyClosure (eval cxt) c (vyBound (envLvl pren.dom))
        TyLam kind <$> go (liftPRen kind pren) t
      VyForall kind c -> do
        t <- evalTyClosure (eval cxt) c (vyBound (envLvl pren.dom))
        TyForall kind <$> go (liftPRen kind pren) t
      VyTuple elems -> do
        TyTuple <$> traverse (go pren) elems
      VyDict dict -> do
        TyDict <$> traverse (go pren) dict
      VyArr icit dom cod -> do
        dom <- go pren dom
        cod <- go pren cod
        pure $ TyArr icit dom cod
      VyFlex m' sp -> do
        when (m == m') mustFail

        sp <- traverse (go pren) sp
        pure $ foldl' TyApp (TyMeta m') sp
      VyRigid head sp -> do
        head <- case head of
          VyBound l0 -> case IntMap.lookup l0 pren.ren of
            Just l -> do
              let i = envLvl pren.dom - l - 1
              pure $ TyVar i
            Nothing -> do
              mustFail
          VyClass u -> do
            pure $ TyClass u
          VyKnown k -> do
            pure $ TyKnown k
        sp <- traverse (go pren) sp
        pure $ foldl' TyApp head sp
