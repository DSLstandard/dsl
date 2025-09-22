[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/cxt_utils.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/elab_kind.dsl"]
  [import "dsl/eval_kind.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/location.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/pexpr.dsl"]
  [import "dsl/pexpr_utils.dsl"]
  [import "dsl/pretty_kind.dsl"]
  [import "dsl/quote_ty.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "dsl/unify_kind.dsl"]
  [import "dsl/unify_or_report.dsl"]
  [import "dsl/unify_ty.dsl"]
  [import "wlp.dsl"]

  [class-decl CheckKind]
  [class-enum CheckKind
    [member Infer ,]
    [member Check Kind]
  ]

  // * Error reporting utils

  [def-decl make_and_mark_error_ty [-> Cxt Expr Kind [Doc Void] Vy]]
  [def-impl make_and_mark_error_ty [\ cxt self kind msg [do
    [Cxt.mark_expr_error cxt self msg]
    [Cxt.fresh_ty_meta cxt kind [MetaReason.new self msg]]
  ]]]

  [def-decl make_and_mark_error_elab_ty [-> Cxt Expr CheckKind [Doc Void] [; Ty Kind]]]
  [def-impl make_and_mark_error_elab_ty [\ cxt self check msg [do
    [let= [; t kind] [match check
      [[Check exp_kind] [do
        [let t [make_and_mark_error_ty cxt self exp_kind [msg . <> [Doc.line] . <> [Doc.of "Expected kind: "] . <> [pretty_kind exp_kind]]]]
        [; t exp_kind]
      ]]
      [[Infer] [do
        [let kind [Cxt.fresh_kind_meta cxt [MetaReason.new self [Doc.of "kind placeholder"]]]]
        [let t [make_and_mark_error_ty cxt self kind [msg . <> [Doc.line] . <> [Doc.of "Placeholder kind: "] . <> [pretty_kind kind]]]]
        [; t kind]
      ]]
    ]]
    [let t [quote_ty [Cxt.eval cxt] [cxt . Cxt.ty_scope . Scope.lvl] t]]
    [; t kind]
  ]]]

  // * Elab ty functions

  [def-decl ty_pat [PExpr [-> Cxt CheckKind [; Ty Kind]]]]

  [def-decl pub elab_ty [-> Cxt Expr CheckKind [; Ty Kind]]]
  [def-impl elab_ty [\ cxt self check [do
    [let result [match_or_report cxt self ty_pat]]
    [match result
      [[None] [make_and_mark_error_elab_ty cxt self check [Doc.of "Bad type expression"]]]
      [[Some elab] [elab cxt check]]
    ]
  ]]]

  [def-decl pub check_ty [-> Cxt Expr Kind Ty]]
  [def-impl check_ty [\ cxt self exp_kind [do
    [elab_ty cxt self [CheckKind.Check exp_kind] . Pair.fst]
  ]]]

  [def-decl pub infer_ty [-> Cxt Expr [; Ty Kind]]]
  [def-impl infer_ty [\ cxt self [do
    [elab_ty cxt self CheckKind.Infer]
  ]]]

  [def-decl on_checked_ty [-> Ty CheckKind [; Ty Kind]]]
  [def-impl on_checked_ty [\ ty check [do
    [match check
      [[Infer] [panic "on_checked_ty called when 'check' = Infer"]]
      [[Check kind] [; ty kind]]
    ]
  ]]]

  [def-decl on_inferred_ty [-> Cxt Expr Ty Kind CheckKind [; Ty Kind]]]
  [def-impl on_inferred_ty [\ cxt self ty got_kind check [do
    [match check
      [[Infer] [do
        [; ty got_kind]
      ]]
      [[Check exp_kind] [do
        [match [unify_kind cxt got_kind exp_kind]
          [[Fail] [do
            [let msg [
              [Doc.of [type Void] "Has kind: "]
              . <> [pretty_kind got_kind]
              . <> [Doc.line]
              . <> [Doc.of "Should be: "]
              . <> [pretty_kind exp_kind]
            ]]
            [make_and_mark_error_elab_ty cxt self check msg]
          ]]
          [[Ok] [do
            [; ty exp_kind]
          ]]
        ]
      ]]
    ]
  ]]]

  [def-decl force_check_kind_weak [-> Cxt CheckKind CheckKind]]
  [def-impl force_check_kind_weak [\ cxt check [do
    [match check
      [[Infer]
        [CheckKind.Infer]]
      [[Check kind]
        [CheckKind.Check [force_kind_weak [Cxt.eval cxt] kind]]]
    ]
  ]]]

  // * Ty patterns

  [def-decl ty_pat.arrow.auto_arr [PExpr [-> Cxt [-> Cxt Ty] Ty]]]
  [def-impl ty_pat.arrow.auto_arr [PExpr.block_ "auto" [
    [ \ ty_expr cxt check_rest [do
      [let dom [check_ty cxt ty_expr Kind.Star]]
      [let cod [check_rest cxt]]
      [Ty.Arr Icit.Auto dom cod]
    ]]
    . <$> [PList.one_of [PExpr.label "<type>" PExpr.any_expr]]
  ]]]

  [def-decl ty_pat.arrow.forall [PExpr [-> Cxt [-> Cxt Ty] Ty]]]
  [def-impl ty_pat.arrow.forall [PExpr.block "type" [
    [ \ [name Name] [kind_expr [Maybe Expr]] block cxt check_rest [do
      [let self [block . PExpr.Block.self]]
      [let kind [match kind_expr
        [[None] [Cxt.fresh_kind_meta cxt [MetaReason.new self [Doc.of "kind placeholder"]]]]
        [[Some kind_expr] [elab_kind cxt kind_expr]]
      ]]
      [let cod [check_rest [cxt . Cxt.extend_ty [Scope.one name kind]]]]
      [Ty.Forall kind cod]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
    . <*> [PList.optional_of [PExpr.label "<kind>" PExpr.any_expr]]
  ]]]

  [def-decl ty_pat.arrow.expl_arr [PExpr [-> Cxt [-> Cxt Ty] Ty]]]
  [def-impl ty_pat.arrow.expl_arr
    [PExpr.any_expr . map [\ ty_expr cxt check_rest [do
      [let dom [check_ty cxt ty_expr Kind.Star]]
      [let cod [check_rest cxt]]
      [Ty.Arr Icit.Expl dom cod]
    ]]]
  ]

  [def-decl ty_pat.arrow.dom_entry [PExpr [-> Cxt [-> Cxt Ty] Ty]]]
  [def-impl ty_pat.arrow.dom_entry [Foldable.mconcat [vec
    ty_pat.arrow.auto_arr
    ty_pat.arrow.forall
    // Must be placed last because of LL parsing
    ty_pat.arrow.expl_arr
  ]]]

  [def-decl ty_pat.arrow [PExpr [-> Cxt CheckKind [; Ty Kind]]]]
  [def-impl ty_pat.arrow [[PExpr.block "->" [
    [\ [entries [Array Expr]] [block PExpr.Block] [in_cxt Cxt] [check CheckKind] [do
      [let* [go [\ [i I32] [cxt Cxt] [do
        [let entry [entries . Array.at i]]
        [if [entries . Array.is_last_ix i]
          [do
            [check_ty cxt entry Kind.Star]
          ]
          [do
            [let run [PExpr.match ty_pat.arrow.dom_entry entry
              . Either.expect "ty_pat.arrow.dom_entry should never fail"]]
            [let check_rest [go [I32.suc i]]]
            [run cxt check_rest]
          ]
        ]
      ]]]]
      [let ty [go 0 in_cxt]]
      [let kind Kind.Star]
      [; ty kind]
    ]]
    . <$> [PList.some_of [PExpr.label "<entry>" PExpr.any_expr]]
  ]]]]

  [def-decl ty_pat.tuple [PExpr [-> Cxt CheckKind [; Ty Kind]]]]
  [def-impl ty_pat.tuple [[PExpr.block ";" [
    [\ elems block cxt check [do
      [let self [block . PExpr.Block.self]]
      [let elems [elems . Array.map [\ elem [do
        [check_ty cxt elem Kind.Star]
      ]]]]
      [on_inferred_ty cxt self [Ty.Tuple elems] Kind.Star check]
    ]]
    . <$> [PList.many_of [PExpr.label "<type>" PExpr.any_expr]]
  ]]]]

  [def-decl ty_pat.var [PExpr [-> Cxt CheckKind [; Ty Kind]]]]
  [def-impl ty_pat.var [
    [PExpr.capture [PExpr.any_sym]] . map [\ input cxt check [do
      [let= [; self name] input]

      // 1. Try resolving as a local type variable
      [if-let [Some [; var_i kind]] [cxt . Cxt.ty_scope . Scope.resolve name] [do
        [on_inferred_ty cxt self [Ty.Var var_i] kind check]
      ]]

      // 2. Try resolving as a struct/enum/tydef reference
      [if-let [Some uid] [cxt . Cxt.lookup_namespace name] [do
        [match [cxt . Cxt.db . Database.get_entry uid]
          [[TyDef tydef] [do
            [on_inferred_ty cxt self [Ty.Def uid] [tydef . XTyDef.kind] check]
          ]]
          [[Class class] [do
            [on_inferred_ty cxt self [Ty.Class uid] [class . XClass.compute_kind] check]
          ]]
          [_ [do
            [let msg [Foldable.mconcat [vec
              [Doc.of [type Void] "Type variable "]
              [Doc.of name . Doc.squotes]
              [Doc.of " is not referencing a struct/enum/tydef"]
            ]]]
            [make_and_mark_error_elab_ty cxt self check msg]
          ]]
        ]
      ]]

      // 3. Try resolving as a known type
      [if-let [Some known] [KnownType.from_name? name] [do
        [let ty [Ty.Known known]]
        [let kind [KnownType.kind known]]
        [on_inferred_ty cxt self ty kind check]
      ]]

      // 4. Try resolving as a known type name (e.g., ',')
      [if-let [true] [== name ","] [do
        [let ty [Ty.Tuple [Array.create ,]]]
        [let kind Kind.Star]
        [on_inferred_ty cxt self ty kind check]
      ]]

      // 5. Unknown type variable
      [make_and_mark_error_elab_ty cxt self check [Foldable.mconcat [vec
        [Doc.of [type Void] "Unknown type variable: "]
        [Doc.of name . Doc.squotes]
      ]]]
    ]]
  ]]

  [def-decl ty_pat.app [PExpr [-> Cxt CheckKind [; Ty Kind]]]]
  [def-impl ty_pat.app [PExpr.list [
    [\ [entries [Array Expr]] [cxt Cxt] [check CheckKind] [do
      // NOTE: Compared to elab_tm's app pattern, this is much simpler to
      // implement since we have less cases to handle (e.g., implicit args)

      [let head_expr [entries . Array.pop_first]]
      [let arg_exprs entries]

      [let arg_exprs_iter [arg_exprs . Array.iter]]
      [let* [go [\ [f_kind Kind] [f Ty] [do
        [else-let [Some x_expr] [arg_exprs_iter . Iter.next?] [do
          // End case
          [on_inferred_ty cxt head_expr f f_kind check]
        ]]

        [let f_kind [force_kind_weak [Cxt.eval cxt] f_kind]]
        [match f_kind
          [[Arr [; exp_x_kind cod]] [do
            // Use 'f_kind' to check
            [let x [check_ty cxt x_expr exp_x_kind]]
            [go cod [Ty.App f x]]
          ]]
          [_ [do
            // Infer for 'f_kind'
            [let= [; x got_x_kind] [infer_ty cxt x_expr]]
            [let cod [Cxt.fresh_kind_meta cxt [MetaReason.new x_expr [Doc.of "return kind placeholder"]]]]

            [let inferred_f_kind [Kind.Arr got_x_kind cod]]

            // FIXME: x_expr is a bad expr_ref for unify_kind_or_report here
            [let u_result [unify_kind_or_report cxt x_expr inferred_f_kind f_kind]]
            [match u_result
              [[Fail] [do
                // If application type is bad, drop this arg for error-tolerance.
                [go f_kind f]
              ]]
              [[Ok] [do
                [go cod [Ty.App f x]]
              ]]
            ]
          ]]
        ]
      ]]]]

      // NOTE: Because we immediately infer the head type, 'check' is not used.
      // This is awkward if the user over-parenthesizes a single type expression
      // `<type>` to be `[<type>]`. We shift the responsibility of not doing
      // this to the user and WILL NOT create a special case to handle it.
      //
      // Note that this phenomenon also exists in 'elab_tm' application.
      [let= [; head_ty head_kind] [infer_ty cxt head_expr]]
      [go head_kind head_ty]
    ]]
    . <$> [PList.some_of [PExpr.label "<type>" PExpr.any_expr]]
  ]]]

  [def-decl ty_pat.lambda.elab_param [
    ->
    [; Expr Name [Maybe Expr]]
    Cxt CheckKind [-> Cxt CheckKind [; Ty Kind]] [; Ty Kind]
  ]]
  [def-impl ty_pat.lambda.elab_param [\ input cxt check check_rest [do
    [let= [; self name dom_kind_expr] input]
    [let check [force_check_kind_weak cxt check]]
    [match check
      [[Check [Arr [; exp_dom_kind exp_cod_kind]]] [do
        // Check against 'check'
        [dom_kind_expr . Maybe.when_some [\ dom_kind_expr [do
          [let got_dom_kind [elab_kind cxt dom_kind_expr]]
          [unit [unify_kind_or_report cxt dom_kind_expr got_dom_kind exp_dom_kind]]
        ]]]

        [let rest_cxt [cxt . Cxt.extend_ty [Scope.one name exp_dom_kind]]]
        [let= [; rest_ty _] [check_rest rest_cxt [CheckKind.Check exp_cod_kind]]]
        [on_checked_ty [Ty.Lam exp_dom_kind rest_ty] check]
      ]]
      [_ [do
        // Infer for 'check'
        [let got_dom_kind [match dom_kind_expr
          [[None] [do
            [Cxt.fresh_kind_meta cxt [MetaReason.new self [Doc.of "kind placeholder"]]]
          ]]
          [[Some dom_kind_expr] [do
            [elab_kind cxt dom_kind_expr]
          ]]
        ]]
        [let rest_cxt [cxt . Cxt.extend_ty [Scope.one name got_dom_kind]]]
        [let= [; rest_ty rest_kind] [check_rest rest_cxt [CheckKind.Infer]]]
        [on_checked_ty [Ty.Lam got_dom_kind rest_ty] check]
      ]]
    ]
  ]]]

  [def-decl ty_pat.lambda.name_only_param_pat [PExpr [-> Cxt CheckKind [-> Cxt CheckKind [; Ty Kind]] [; Ty Kind]]]]
  [def-impl ty_pat.lambda.name_only_param_pat [
    [\ [input [; Expr Name]] [do
      [let= [; self name] input]
      [ty_pat.lambda.elab_param [; self name [None]]]
    ]]
    . <$> [PExpr.label "<name>" [PExpr.capture PExpr.any_sym]]
  ]]

  [def-decl ty_pat.lambda.annotated_param_pat [PExpr [-> Cxt CheckKind [-> Cxt CheckKind [; Ty Kind]] [; Ty Kind]]]]
  [def-impl ty_pat.lambda.annotated_param_pat [PExpr.captures [PExpr.list [
    [\ [name Name] [kind_expr Expr] [self Expr] [do
      [ty_pat.lambda.elab_param [; self name [Some kind_expr]]]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
    . <*> [PList.one_of [PExpr.label "<kind>" PExpr.any_expr]]
  ]]]]

  [def-decl ty_pat.lambda.param_pat [PExpr [-> Cxt CheckKind [-> Cxt CheckKind [; Ty Kind]] [; Ty Kind]]]]
  [def-impl ty_pat.lambda.param_pat [Foldable.mconcat [vec
    ty_pat.lambda.name_only_param_pat
    ty_pat.lambda.annotated_param_pat
  ]]]

  [def-decl ty_pat.lambda [PExpr [-> Cxt CheckKind [; Ty Kind]]]]
  [def-impl ty_pat.lambda [PExpr.block_ "\\" [
    [\ [in_entries [Array Expr]] [in_cxt Cxt] [in_check CheckKind] [do
      [let* [go [\ [i I32] [cxt Cxt] [check CheckKind] [do
        [let entry [in_entries . Array.at i]]

        [if-let [true] [in_entries . Array.is_last_ix i] [do
          // Last entry must be the type lambda's body
          [elab_ty cxt entry check]
        ]]

        // Entries before the last are parameters
        [match [match_or_report cxt entry ty_pat.lambda.param_pat]
          [[None] [do
            // Skip bad lambda param for error-tolerance
            [go [I32.suc i] cxt check]
          ]]
          [[Some elab] [do
            [elab cxt check [go [I32.suc i]]]
          ]]
        ]
      ]]]]

      [go 0 in_cxt in_check]
    ]]
    . <$> [PList.some_of [PExpr.label "<entry>" PExpr.any_expr]]
  ]]]

  [def-decl ty_pat.dict [PExpr [-> Cxt CheckKind [; Ty Kind]]]]
  [def-impl ty_pat.dict [PExpr.block "Dict" [
    [\ [in_entries [Array Expr]] [block PExpr.Block] [cxt Cxt] [check CheckKind] [do
      [let self [block . PExpr.Block.self]]

      [let entry_pat [PExpr.list [
        [Pair.new]
        . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
        . <*> [PList.one_of [PExpr.label "<type>" PExpr.any_expr]]
      ]]]

      [let dict_fields [StringMutMap.create ,]]
      [in_entries . Array.foreach [\ in_entry [do
        [let result [match_or_report cxt in_entry entry_pat]]
        [else-let [Some [; name ty]] result continue]
        [let ty [check_ty cxt ty Kind.Star]]
        [dict_fields . StringMutMap.set name ty]
        continue
      ]]]
      [let dict_ty [Ty.Dict [dict_fields . StringMutMap.freeze]]]
      [on_inferred_ty cxt self dict_ty Kind.Star check]
    ]]
    . <$> [PList.many_of [PExpr.label "<entry>" PExpr.any_expr]]
  ]]]

  [def-impl ty_pat [Foldable.mconcat [vec
    ty_pat.tuple
    ty_pat.arrow
    ty_pat.lambda
    ty_pat.dict
    ty_pat.app
    ty_pat.var
  ]]]
]