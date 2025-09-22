[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/elab_kind.dsl"]
  [import "dsl/elab_ty.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/pexpr.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "wlp.dsl"]

  [def-decl ref_class_or_report [-> Cxt Expr Name [Maybe XClass]]]
  [def-impl ref_class_or_report [\ cxt name_expr name [do
    [else-let [Some uid] [cxt . Cxt.lookup_namespace name] [do
      [let msg [[Doc.of [type Void] "Unknown class declaration: "] . <> [Doc.squotes [Doc.of name]]]]
      [cxt . Cxt.mark_expr_error name_expr msg]
      [None]
    ]]

    [else-let [Class class] [cxt . Cxt.db . Database.get_entry uid] [do
      [cxt . Cxt.mark_expr_error name_expr [Doc.of "Should refer to a class declaration"]]
      [None]
    ]]

    [Some class]
  ]]]

  [def-decl pub pat_class_decl [PExpr [-> Cxt ,]]]
  [def-impl pat_class_decl [PExpr.block_ "class-decl" [
    [\ [name Name] [generic_entries [Array [; Name Expr]]] [cxt Cxt] [do
      [let generic_scope [var [Scope.nil]]]

      // Check generic parameters from `[class-decl MyEnum [...generics]]`
      [generic_entries . Array.foreach [\ entry [do
        [let= [; name kind_expr] entry]
        [let kind [elab_kind cxt kind_expr]]
        [set generic_scope [[get generic_scope] . Scope.push name kind]]
        continue
      ]]]

      [let uid [cxt . Cxt.db . Database.fresh_uid]]
      [let class [XClass.new* [dict
        [uid uid]
        [name name]
        [generics [get generic_scope]]
        [members [Array.create ,]]
      ]]]
      [cxt . Cxt.db . Database.insert_entry [XEntry.Class class]]
      [cxt . Cxt.export_name name uid]
      [cxt . Cxt.open_name name uid]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
    . <*> [PList.many_of [PExpr.list [
      Pair.new
      . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
      . <*> [PList.one_of [PExpr.label "<kind>" PExpr.any_expr]]
    ]]]
  ]]]

  [def-decl pub pat_class_enum [PExpr [-> Cxt ,]]]
  [def-impl pat_class_enum [PExpr.block_ "class-enum" [
    [\ [in_name [; Expr Name]] [in_members [Array [; Name Expr]]] [cxt Cxt] [do
      [let= [; name_expr name] in_name]
      [let= [Some class] [ref_class_or_report cxt name_expr name] [_ ,]]

      // Build all enum members of the class
      [let class_cxt [cxt . Cxt.extend_ty [class . XClass.generics]]]
      [in_members . Array.iter . Iter.ixed . Iter.foreach [\ input [do
        [let= [; member_id in_member] input]
        [let= [; member_name value_ty_expr] in_member]
        [let value_ty [check_ty class_cxt value_ty_expr Kind.Star]]
        [let member [XMember.new* [dict
          [id member_id]
          [name member_name]
          [ty value_ty]
        ]]]
        [class . XClass.members . Array.append member]

        continue
      ]]]

      // `[MyClass ...generics]` (Assumed to be scoped under [-> [type ...Generics])
      [let class_ty
        [Ty.app_spine [Ty.Class [class . XClass.uid]]
          [Spine.of_ty_vars [class . XClass.generics . Scope.lvl]]]]

      // Create the '.new' constructor for each enum member
      [class . XClass.members . Array.foreach [\ member [do
        [let con_name [Foldable.mconcat [vec name "." [member . XMember.name] ".new"]]]
        [let con_uid [cxt . Cxt.db . Database.fresh_uid]]
        [let con_def [XDef.new_simple* [dict
          [uid con_uid]
          [name con_name]
          [ty [TyScope.fold_as_forall [class . XClass.generics] [Ty.arr [member . XMember.ty] class_ty]]]
          [tm [Tm.Lam [Tm.Class [class . XClass.uid] [member . XMember.id] [Tm.Var 0]]]]
        ]]]
        [cxt . Cxt.db . Database.insert_entry [XEntry.Def con_def]]

        // * Register into namespace
        [cxt . Cxt.export_name con_name con_uid]
        [cxt . Cxt.open_name con_name con_uid]

        continue
      ]]]

      // Create the curried constructor for each enum member for ergonomic use
      //
      // If the member's value type is a tuple, curry on that.
      //
      // If the member's value type is not a tuple, the constructor's
      // implementation is the same as '.new', to act as an alias to '.new'.
      [class . XClass.members . Array.foreach [\ member [do
        [let con_name [Foldable.mconcat [vec name "." [member . XMember.name]]]]
        [let con_uid [cxt . Cxt.db . Database.fresh_uid]]

        // TODO: Normalize member.ty first to evaluate tydefs if the user uses
        // it?
        [let= [; con_ty con_tm] [match [member . XMember.ty]
          [[Tuple elem_tys] [do
            [let num_elems [elem_tys . Array.length]]
            [;
              [TyScope.fold_as_forall [class . XClass.generics] [Ty.fold_expl_arr [elem_tys . Array.iter] class_ty]]
              [Tm.fold_lam num_elems [Tm.Class
                [class . XClass.uid]
                [member . XMember.id]
                [Tm.Tuple [Spine.of_tm_vars num_elems . Spine.to_array]]
              ]]
            ]
          ]]
          [_ [do
            [;
              [TyScope.fold_as_forall [class . XClass.generics] [Ty.arr [member . XMember.ty] class_ty]]
              [Tm.Lam [Tm.Class [class . XClass.uid] [member . XMember.id] [Tm.Var 0]]]
            ]
          ]]
        ]]
        [let con_def [XDef.new_simple* [dict
          [uid con_uid] [name con_name] [ty con_ty] [tm con_tm]]]]
        [cxt . Cxt.db . Database.insert_entry [XEntry.Def con_def]]

        // * Register into namespace
        [cxt . Cxt.export_name con_name con_uid]
        [cxt . Cxt.open_name con_name con_uid]

        continue
      ]]]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" [PExpr.capture PExpr.any_sym]]]
    . <*> [PList.many_of [PExpr.block_ "member" [
      Pair.new
      . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
      . <*> [PList.one_of [PExpr.label "<value-type>" PExpr.any_expr]]
    ]]]
  ]]]

  [def-decl pub pat_class_struct [PExpr [-> Cxt ,]]]
  [def-impl pat_class_struct [PExpr.block_ "class-struct" [
    [\ [in_name [; Expr Name]] [in_fields [Array [; Name Expr]]] [cxt Cxt] [do
      [let= [; name_expr name] in_name]
      [let= [Some class] [ref_class_or_report cxt name_expr name] [_ ,]]

      // Process struct fields
      [let class_cxt [cxt . Cxt.extend_ty [class . XClass.generics]]]
      [let num_fields [in_fields . Array.length]]

      [let field_names [Array.create ,]]
      [let field_tys [Array.create ,]]

      [in_fields . Array.foreach [\ in_field [do
        [let= [; field_name field_ty_expr] in_field]
        [let field_ty [check_ty class_cxt field_ty_expr Kind.Star]]

        [field_names . Array.append field_name]
        [field_tys . Array.append field_ty]
        continue
      ]]]

      // Implement the class, which is one single member named 'mk' (as in
      // "make"; with MemberID = 0) housing all fields in a tuple
      //
      // We dedicate the name 'new' to be a constructor that is a curried
      // version of the 'mk' constructor for ergonomic use (e.g., <$> <*>
      // chains). 'new*' is the dict version of the constructor.
      [let member [XMember.new* [dict
        [id 0]
        [name "mk"]
        [ty [Ty.Tuple field_tys]]
      ]]]
      [class . XClass.members . Array.append member]

      // `[MyClass ...generics]` (Assumed to be scoped under [-> [type ...Generics])
      [let class_ty
        [Ty.app_spine [Ty.Class [class . XClass.uid]]
          [Spine.of_ty_vars [class . XClass.generics . Scope.lvl]]]]

      // FIXME: Make the '.mk' constructor def

      // Create the '.new' constructor
      [do
        [let con_name [Foldable.mconcat [vec name ".new"]]]
        [let con_uid [cxt . Cxt.db . Database.fresh_uid]]
        [let con_def [XDef.new_simple* [dict
          [uid con_uid]
          [name con_name]
          [ty [TyScope.fold_as_forall [class . XClass.generics] [Ty.fold_expl_arr [field_tys . Array.iter] class_ty]]]
          [tm [Tm.fold_lam num_fields [Tm.Class
            [class . XClass.uid]
            [member . XMember.id]
            [Tm.Tuple [Spine.of_tm_vars num_fields . Spine.to_array]]
          ]]]
        ]]]
        [cxt . Cxt.db . Database.insert_entry [XEntry.Def con_def]]

        // * Register into namespace
        [cxt . Cxt.export_name con_name con_uid]
        [cxt . Cxt.open_name con_name con_uid]
      ]

      // Create the '.new*' constructor
      [do
        [let con_name [Foldable.mconcat [vec name ".new*"]]]
        [let con_uid [cxt . Cxt.db . Database.fresh_uid]]

        [let in_dict_ty_fields [StringMutMap.create [type Ty] ,]]
        [let in_tuple_tm_elems [Array.create ,]]
        [loop<n num_fields [\ i [do
          [in_dict_ty_fields . StringMutMap.set
            [field_names . Array.at i]
            [field_tys . Array.at i]
          ]
          [in_tuple_tm_elems . Array.append [Tm.GetDictKey
            [Tm.Var 0] // Points to the 'Tm.Lam' of the constructor.
            [field_names . Array.at i]
          ]]
          continue
        ]]]
        [let in_dict_ty [Ty.Dict [StringMutMap.freeze in_dict_ty_fields]]]

        [let con_def [XDef.new_simple* [dict
          [uid con_uid]
          [name con_name]
          [ty [TyScope.fold_as_forall [class . XClass.generics] [Ty.arr in_dict_ty class_ty]]]
          [tm [Tm.Lam [Tm.Class
            [class . XClass.uid]
            [member . XMember.id]
            [Tm.Tuple in_tuple_tm_elems]
          ]]]
        ]]]
        [cxt . Cxt.db . Database.insert_entry [XEntry.Def con_def]]

        // * Register into namespace
        [cxt . Cxt.export_name con_name con_uid]
        [cxt . Cxt.open_name con_name con_uid]
      ]

      // *** Create field getters/setter functions

      // withValueTm struct_tm f = ```haskell
      //   case struct_tm of
      //     (mk fields_tuple) -> f fields_tuple
      // ```
      //
      // The match tree is hand-crafted
      [let with_value_tm [\ [struct_tm Tm] [f [-> Tm Tm]]
        [Tm.Match [TmMatch.Tree.new* [dict
          [root_scrut_tm struct_tm]
          [root_node [TmMatch.Node.SplitOn
            0 // Targets 'root_scrut_tm', so 'struct_tm'
            [TmMatch.Split.Class [member . XMember.id] [Array.one
              [TmMatch.Split.Class.Branch.new* [dict
                [member_id [member . XMember.id]]
                [next_node [TmMatch.Node.Leaf [TmMatch.Leaf.new* [dict
                  [target_clause_id 0] // The 1st and ONLY match clause
                  [pat_env [Env.one 1]] // Pat env: [member's value]
                  [on_guard_fail [None]]
                ]]]]
              ]]
            ]]
          ]]
          [clauses [Array.one [TmMatch.Clause.new* [dict
            [id 0] // The 1st and ONLY match clause
            [pat_env_lvl 1] // Because - Pat env: [member's value]
            [body_tm [f [Tm.Var 0]]] // (TmVar 0) refers to the member's value
            [guard_tm [None]]
          ]]]]
        ]]]
      ]]

      // Create field getters
      [loop<n num_fields [\ i [do
        [let field_name [field_names . Array.at i]]
        [let field_ty [field_tys . Array.at i]]

        [let fn_uid [cxt . Cxt.db . Database.fresh_uid]]
        [let fn_name [Foldable.mconcat [vec [class . XClass.name] "." field_name]]]
        [let fn_def [XDef.new_simple* [dict
          [uid fn_uid]
          [name fn_name]

          // @[-> [type ...Generics] [MyClass ...Generics] field_ty]@
          [ty [TyScope.fold_as_forall [class . XClass.generics] [Ty.arr class_ty field_ty]]]

          [tm [Tm.Lam
            [with_value_tm [Tm.Var 0]
              [\ value_tm [Tm.GetTupleIndex value_tm i]]
            ]
          ]]
        ]]]
        [cxt . Cxt.db . Database.insert_entry [XEntry.Def fn_def]]

        // * Register into namespace
        [cxt . Cxt.export_name fn_name fn_uid]
        [cxt . Cxt.open_name fn_name fn_uid]

        continue
      ]]]

      // Create field setters
      [loop<n num_fields [\ i [do
        [let field_name [field_names . Array.at i]]
        [let field_ty [field_tys . Array.at i]]

        [let fn_uid [cxt . Cxt.db . Database.fresh_uid]]
        [let fn_name [Foldable.mconcat [vec [class . XClass.name] ".=" field_name]]]
        [let fn_def [XDef.new_simple* [dict
          [uid fn_uid]
          [name fn_name]

          // @[-> [type ...Generics] [MyClass ...Generics] field_ty [MyClass ...Generics]]@
          [ty [TyScope.fold_as_forall [class . XClass.generics] [Ty.fold_expl_arr [Iter.two class_ty field_ty] class_ty]]]

          // Haskell: λ struct new_field_value ->
          //   case struct of
          //     (MyStruct.mk fields_tuple) -> (MyStruct.mk (set _1 new_field_value fields_tuple)
          [tm [Tm.Lam [Tm.Lam
            [with_value_tm [Tm.Var 1] [\ value_tm
              [Tm.Class [class . XClass.uid] [member . XMember.id]
                // 'TmVar 1' to refer the 'new_value' (NOTE: we have
                // to account for the 'value_tuple' bound by 'with_value_tm'
                [Tm.SetTupleIndex value_tm i [Tm.Var 1]]
              ]
            ]]
          ]]]
        ]]]
        [cxt . Cxt.db . Database.insert_entry [XEntry.Def fn_def]]

        // * Register into namespace
        [cxt . Cxt.export_name fn_name fn_uid]
        [cxt . Cxt.open_name fn_name fn_uid]

        continue
      ]]]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" [PExpr.capture PExpr.any_sym]]]
    . <*> [PList.many_of [PExpr.block_ "field" [
      Pair.new
      . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
      . <*> [PList.one_of [PExpr.label "<type>" PExpr.any_expr]]
    ]]]
  ]]]
]