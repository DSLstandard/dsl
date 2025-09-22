module DSL.ProcessClass where

import DSL.Cxt
import DSL.Cxt qualified as Cxt
import DSL.Database
import DSL.Dict qualified as Dict
import DSL.ElabKind
import DSL.ElabTy
import DSL.Types
import DSL.Utils
import DSL.Expr
import DSL.Pat qualified as P
import Data.List qualified as List
import Data.Traversable
import Data.Vector qualified as V
import Optics
import Prettyprinter qualified as PP
import Relude

refClassOrReport :: Cxt -> Expr -> Name -> IO (Maybe XClass)
refClassOrReport cxt nameExpr name = do
  uid <- lookupNamespace cxt name
  case uid of
    Nothing -> do
      Cxt.markExprError cxt nameExpr $ "Unknown class declaration: " <> PP.pretty name
      pure Nothing
    Just uid -> do
      enum <- queryClass cxt.db uid
      pure (Just enum)

patClassDecl :: P.PExpr (Cxt -> IO ())
patClassDecl = do
  P.block_ "class-decl" $
    ( \name generics cxt -> do
        uid <- generateUID cxt.db

        generics :: TyScope <-
          scopeFromPushMany <$> for generics \(name, kindExpr) -> do
            kind <- elabKind cxt kindExpr
            pure (name, kind)

        insertEntry cxt.db $ XEntry'Class XClass{uid, name, generics, members = V.empty}

        addToNamespace cxt name uid
    )
      <$> P.item (P.label "<name>" P.anySym)
      <*> P.manyItem
        ( P.list $
            (,)
              <$> P.item (P.label "<name>" P.anySym)
              <*> P.item (P.label "<kind>" P.anyExpr)
        )

data ClassMemberParams = ClassMemberParams
  { name :: Name
  , ty :: Ty
  }

-- | Populate the 'members' of the class's 'XClass' entry.
implementClassMembers :: Cxt -> UID -> [(Name, Ty)] -> IO ()
implementClassMembers cxt uid members0 = do
  cls <- queryClass cxt.db uid

  let
    members = flip imap members0 \(MemberID -> id) (name, valueType) ->
      Member{id = id, name = name, ty = valueType}

  -- Update the class entry with the members
  insertEntry cxt.db $ XEntry'Class (cls & set #members (V.fromList members))

patClassEnum :: P.PExpr (Cxt -> IO ())
patClassEnum = do
  P.block_ "class-enum" $
    ( \(clsNameExpr, clsName) inMembers cxt -> do
        whenJustM (refClassOrReport cxt clsNameExpr clsName) \cls -> do
          implementClassMembers cxt cls.uid
            =<< for inMembers \(memberName, valueTypeExpr) -> do
              let valueCxt = cxtExtendTy cls.generics cxt
              valueType <- checkTy valueCxt valueTypeExpr KStar
              pure (memberName, valueType)

          -- To refresh 'cls.members' after 'implementClassMembers' updated the
          -- database entry.
          cls <- queryClass cxt.db cls.uid

          -- @clsTy = [Enum ...Generics]@ (Assumed to be scoped under [-> [type ...Generics])
          let clsTy = tyFoldApp (TyClass cls.uid) (tyVarSp (scopeLvl cls.generics))

          -- Create constructors for each enum member
          for_ cls.members \member -> do
            -- ### Create the '.new' constructor
            do
              conUID <- generateUID cxt.db
              let conName = cls.name <> ".new"
              insertEntry cxt.db . XEntry'Def $
                xdefSimple
                  conUID
                  conName
                  ( tyScopeFoldAsForalls cls.generics $
                      tyArr member.ty $
                        tyFoldApp (TyClass cls.uid) (tyVarSp (scopeLvl cls.generics))
                  )
                  ( TmLam $
                      TmClass cls.uid member.id (TmVar 0)
                  )
              Cxt.addToNamespace cxt conName conUID

            -- ### Create the curried constructor for ergonomic use
            --
            -- If the member's value type is a tuple, curry on that.
            --
            -- If the member's value type is not a tuple, the constructor's
            -- implementation is the same as '.new', to act as an alias to
            -- '.new'.

            do
              conUID <- generateUID cxt.db
              let conName = cls.name <> "." <> member.name

              let
                -- TODO: Normalize member.ty first to evaluate tydefs if the
                -- user uses it?
                (conTy, conTm) = case member.ty of
                  TyTuple fieldTypes -> do
                    let numFields = length fieldTypes
                    ( -- @[-> [type ...Generics] [... Fields] [Struct ...Generics]]@
                      tyScopeFoldAsForalls cls.generics $
                        tyFoldArr (toList fieldTypes) $
                          clsTy
                      , tmFoldLams numFields $
                          TmClass cls.uid member.id $
                            TmTuple $
                              tmVarSp numFields
                      )
                  _ -> do
                    ( tyScopeFoldAsForalls cls.generics $
                        tyArr member.ty $
                          tyFoldApp (TyClass cls.uid) (tyVarSp (scopeLvl cls.generics))
                      , TmLam $
                          TmClass cls.uid member.id (TmVar 0)
                      )
              insertEntry cxt.db . XEntry'Def $
                xdefSimple conUID conName conTy conTm
              Cxt.addToNamespace cxt conName conUID
    )
      <$> P.item (P.label "<name>" (P.capture P.anySym))
      <*> P.manyItem
        ( P.block_
            "member"
            ( (,)
                <$> P.item (P.label "<name>" P.anySym)
                <*> P.item (P.label "<value-type>" P.anyExpr)
            )
        )

patClassStruct :: P.PExpr (Cxt -> IO ())
patClassStruct = do
  P.block_ "class-struct" $
    ( \(clsNameExpr, clsName) inFields cxt -> do
        whenJustM (refClassOrReport cxt clsNameExpr clsName) \cls -> do
          let numFields = length inFields
          let (fieldNames, fieldTyExprs) = unzip inFields
          let fieldIxes :: [Int] = [0 .. (numFields - 1)]

          -- Check field tys
          fieldTys <- for fieldTyExprs \tyExpr -> do
            checkTy (cxtExtendTy cls.generics cxt) tyExpr KStar

          -- Implement the class, which is one single member named 'mk' (as in
          -- "make"; with MemberID = 0) housing all fields in a tuple
          --
          -- We dedicate the name 'new' to be a constructor that is a curried
          -- version of the 'mk' constructor for ergonomic use (e.g., <$> <*>
          -- chains). 'new*' is the dict version of the constructor.
          implementClassMembers cxt cls.uid [("mk", TyTuple (V.fromList fieldTys))]

          -- @clsTy = [Struct ...Generics]@ (Assumed to be scoped under [-> [type ...Generics])
          let clsTy = tyFoldApp (TyClass cls.uid) (tyVarSp (scopeLvl cls.generics))

          -- FIXME: Make the '.mk' constructor def

          -- ### Create constructor '.new'
          do
            let conName = cls.name <> ".new"
            conUID <- generateUID cxt.db
            insertEntry cxt.db . XEntry'Def $
              xdefSimple
                conUID
                conName
                ( -- @[-> [type ...Generics] [... Fields] [Struct ...Generics]]@
                  tyScopeFoldAsForalls cls.generics $
                    tyFoldArr fieldTys $
                      clsTy
                )
                ( tmFoldLams numFields $
                    TmClass cls.uid (MemberID 0) $
                      TmTuple $
                        tmVarSp numFields
                )
            Cxt.addToNamespace cxt conName conUID

          -- ### Create constructor '.new*'
          do
            let conName = cls.name <> ".new*"
            conUID <- generateUID cxt.db
            let dictTy = TyDict $ Dict.fromList $ zip fieldNames fieldTys
            insertEntry cxt.db . XEntry'Def $
              xdefSimple
                conUID
                conName
                ( -- @[-> [type ...Generics] [Dict ...Fields] [Struct ...Generics]]@
                  tyScopeFoldAsForalls cls.generics $
                    tyFoldArr [dictTy] $
                      clsTy
                )
                ( TmLam $
                    TmClass cls.uid (MemberID 0) $
                      TmTuple $
                        TmGetDictKey (TmVar 0) <$> fieldNames
                )
            Cxt.addToNamespace cxt conName conUID

          let
            -- @makeFieldFnTy cod = [-> [type ...Generics] [Struct ...Generics] cod]@
            makeFieldFnTy cod =
              tyScopeFoldAsForalls cls.generics $
                tyFoldArr [clsTy] $
                  cod

            -- withValueTm struct_tm f = ```haskell
            --   case struct_tm of
            --     (mk fields_tuple) -> f fields_tuple
            -- ```
            --
            -- The match tree is hand-crafted
            withValueTm :: Tm -> (Tm -> Tm) -> Tm
            withValueTm structTm f =
              TmMatch
                MatchTree
                  { scrutinee = structTm
                  , root =
                      MatchNode'BranchClass 0 $
                        [ MatchBranchClass
                            { memberID = MemberID 0
                            , next =
                                MatchNode'Leaf
                                  MatchLeaf
                                    { targetClauseID = MatchClauseID 0
                                    , patEnv = envOne 1 -- Pat env: [member's value]
                                    , onGuardFail = Nothing
                                    }
                            }
                        ]
                  , clauses =
                      [ MatchClause
                          { id = MatchClauseID 0
                          , patEnvLvl = 1 -- Pat env: [member's value]
                          , bodyTm = f (TmVar 0) -- (TmVar 0) refers to the member's value
                          , guardTm = Nothing
                          }
                      ]
                  }

          for_ (List.zip3 fieldIxes fieldNames fieldTys) \(fieldIx, fieldName, fieldTy) -> do
            let fnName = clsName <> "." <> fieldName
            fnUID <- generateUID cxt.db

            -- Create field getter
            insertEntry cxt.db . XEntry'Def $
              xdefSimple
                fnUID
                fnName
                ( -- @[-> [type ...Generics] [Struct ...Generics] fieldTy]@
                  makeFieldFnTy fieldTy
                )
                ( TmLam $
                    withValueTm (TmVar 0) $
                      \value -> TmGetTupleIndex value fieldIx
                )
            Cxt.addToNamespace cxt fnName fnUID

            -- Create field setter
            for_ (List.zip3 fieldIxes fieldNames fieldTys) \(fieldIx, fieldName, fieldTy) -> do
              let immutName = clsName <> ".=" <> fieldName
              immutUID <- generateUID cxt.db
              insertEntry cxt.db . XEntry'Def $
                xdefSimple
                  immutUID
                  immutName
                  ( -- @[-> [type ...Generics] [Struct ...Generics] fieldTy [Struct ...Generics]]@
                    makeFieldFnTy $ tyFoldArr [fieldTy] clsTy
                  )
                  ( -- Haskell: λ struct new_field_value ->
                    --   case struct of
                    --     (MyStruct.mk fields_tuple) -> (MyStruct.mk (set _1 new_field_value fields_tuple)
                    TmLam $
                      TmLam $
                        withValueTm (TmVar 1) $ \value ->
                          TmClass cls.uid (MemberID 0) $
                            -- 'TmVar 1' to refer the 'new_value' (NOTE: we have
                            -- to account for the 'fields_tuple' bound by 'with_value_tm'
                            TmSetTupleIndex value fieldIx (TmVar 1)
                  )
              Cxt.addToNamespace cxt immutName immutUID
    )
      <$> P.item (P.label "<name>" (P.capture P.anySym))
      <*> P.manyItem
        ( P.block_
            "field"
            ( (,)
                <$> P.item (P.label "<name>" P.anySym)
                <*> P.item (P.label "<type>" P.anyExpr)
            )
        )
