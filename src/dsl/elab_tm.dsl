[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/cxt_utils.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/elab_kind.dsl"]
  [import "dsl/elab_ty.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/instance_resolution.dsl"]
  [import "dsl/mark_tail_calls.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/pexpr.dsl"]
  [import "dsl/pexpr_utils.dsl"]
  [import "dsl/pmatch.dsl"]
  [import "dsl/pretty_ty.dsl"]
  [import "dsl/quote_ty.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "dsl/unify_kind.dsl"]
  [import "dsl/unify_or_report.dsl"]
  [import "dsl/unify_ty.dsl"]
  [import "wlp.dsl"]

  [class-decl CheckTy]
  [class-enum CheckTy
    [member Infer ,]
    [member Check Vy]
  ]

  // * Error reporting utils

  [def-decl make_and_mark_error_tm [-> Cxt Expr [Doc Void] Tm]]
  [def-impl make_and_mark_error_tm [\ cxt self msg [do
    [Cxt.mark_expr_error cxt self msg]
    Tm.Error
  ]]]

  [def-decl make_and_mark_error_elab_tm [-> Cxt Expr CheckTy [Doc Void] [; Tm Vy]]]
  [def-impl make_and_mark_error_elab_tm [\ cxt self check msg [do
    [let t [match check
      [[Check exp_ty] [do
        exp_ty
      ]]
      [[Infer] [do
        [Cxt.fresh_ty_meta cxt Kind.Star [MetaReason.new self [Doc.of "type placeholder"]]]
      ]]
    ]]
    [let x [make_and_mark_error_tm cxt self msg]]
    [; x t]
  ]]]

  // * Elab tm functions

  [def-decl tm_pat [PExpr [-> Cxt CheckTy [; Tm Vy]]]]

  [def-decl pub elab_tm [-> Cxt Expr CheckTy [; Tm Vy]]]
  [def-impl elab_tm [\ cxt self check [do
    [let result [match_or_report cxt self tm_pat]]
    [match result
      [[None] [make_and_mark_error_elab_tm cxt self check [Doc.of "Bad term expression"]]]
      [[Some elab] [elab cxt check]]
    ]
  ]]]

  [def-decl pub check_tm [-> Cxt Expr Vy Tm]]
  [def-impl check_tm [\ cxt self exp_ty [do
    [elab_tm cxt self [CheckTy.Check exp_ty] . Pair.fst]
  ]]]

  [def-decl pub infer_tm [-> Cxt Expr [; Tm Vy]]]
  [def-impl infer_tm [\ cxt self [do
    [elab_tm cxt self CheckTy.Infer]
  ]]]

  [def-decl force_check_ty_weak [-> Cxt CheckTy CheckTy]]
  [def-impl force_check_ty_weak [\ cxt check
    [match check
      [[Infer]
        [CheckTy.Infer]]
      [[Check ty]
        [CheckTy.Check [force_ty_weak [Cxt.eval cxt] ty]]]
    ]
  ]]

  [def-decl on_checked_tm [-> Tm CheckTy [; Tm Vy]]]
  [def-impl on_checked_tm [\ got_tm check
    [match check
      [[Check exp_ty] [; got_tm exp_ty]]
      [[Infer] [panic "on_checked_tm called with Infer"]]
    ]
  ]]

  [def-decl on_inferred_tm [-> Cxt Expr Tm Vy CheckTy [; Tm Vy]]]
  [def-impl on_inferred_tm [\ cxt self got_tm got_ty check
    [match check
      [[Check exp_ty]
        [match [unify_ty cxt got_ty exp_ty]
          [[Fail] [do
            [let got_ty_doc [Cxt.render_vy cxt [force_ty [Cxt.eval cxt] got_ty]]]
            [let exp_ty_doc [Cxt.render_vy cxt [force_ty [Cxt.eval cxt] exp_ty]]]
            [make_and_mark_error_elab_tm cxt self check
              [Foldable.mconcat [vec
                [Doc.of "Has type: "]
                got_ty_doc
                [Doc.line]
                [Doc.of "Should be: "]
                exp_ty_doc
              ]]
            ]
          ]]
          [[Ok] [do
            [; got_tm exp_ty]
          ]]
        ]
      ]
      [[Infer]
        [; got_tm got_ty]
      ]
    ]
  ]]

  // * Tm pattern utils

  [class-decl RawUserClause]
  [class-struct RawUserClause
    [field pat_expr Expr]
    [field guard_expr [Maybe Expr]]
    [field body_expr Expr]
  ]

  [def-decl tm_pat.common_user_clause_pat [PList RawUserClause]]
  [def-impl tm_pat.common_user_clause_pat [
    RawUserClause.new
    . <$> [PList.one_of [PExpr.label "<pat>" PExpr.any_expr]]
    . <*> [PList.optional_of [PExpr.block_ "where" [PList.one_of [PExpr.label "<guard>" PExpr.any_expr]]]]
    . <*> [PList.one_of [PExpr.label "<body>" PExpr.any_expr]]
  ]]

  // * Tm pattern: [; ...]

  [def-decl tm_pat.tuple [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.tuple [PExpr.block ";" [
    PList.rest . map [\ elem_exprs input cxt check [do
      [let self [input . PExpr.Block.self]]

      [let check [force_check_ty_weak cxt check]]
      [match check
        // Use the expected Tuple type if exists
        //
        // However, if the number of entries don't line up, let it fallback and
        // fail inevitably.
        [[Check [Tuple exp_elem_tys]] [where [== [Array.length exp_elem_tys] [Array.length elem_exprs]]] [do
          [let got_elems [Array.create ,]]
          [loop<n [Array.length elem_exprs] [\ i [do
            [let exp_elem_ty [exp_elem_tys . Array.at i]]
            [let elem_expr [elem_exprs . Array.at i]]
            [let elem [check_tm cxt elem_expr exp_elem_ty]]
            [got_elems . Array.append elem]
            continue
          ]]]
          [on_checked_tm [Tm.Tuple got_elems] check]
        ]]
        [_ [do
          [let got_elems [Array.create ,]]
          [let got_elem_tys [Array.create ,]]
          [elem_exprs . Array.foreach [\ elem_expr [do
            [let= [; got_elem got_elem_ty] [infer_tm cxt elem_expr]]
            [got_elems . Array.append got_elem]
            [got_elem_tys . Array.append got_elem_ty]
            continue
          ]]]
          [on_inferred_tm cxt self [Tm.Tuple got_elems] [Vy.Tuple got_elem_tys] check]
        ]]
      ]
    ]]
  ]]]

  // * Tm pattern: [vec ...]

  [def-decl tm_pat.vec [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.vec [PExpr.block "vec" [
    PList.rest . map [\ elem_exprs input cxt check [do
      [let self [input . PExpr.Block.self]]

      [let check [force_check_ty_weak cxt check]]
      [match check
        // Use the expected vec type if exists
        [[Check [Rigid [; [Known Vec] sp]]] [do
          [let exp_item_type [sp . Spine.at 0]]
          [let got_elems [elem_exprs . Array.map [\ elem_expr [do
            [check_tm cxt elem_expr exp_item_type]
          ]]]]
          [on_checked_tm [Tm.Vec got_elems] check]
        ]]

        // No expected type or expected type is not a vec. We have to infer the
        // item type ourselves.
        [_ [do
          [let got_item_type [Cxt.fresh_ty_meta cxt Kind.Star [MetaReason.new self [Doc.of "Vec item type hole"]]]]
          [let got_elems [elem_exprs . Array.map [\ elem_expr [do
            [check_tm cxt elem_expr got_item_type]
          ]]]]
          [let got_ty [Vy.vec got_item_type]]
          [on_inferred_tm cxt self [Tm.Vec got_elems] got_ty check]
        ]]
      ]
    ]]
  ]]]

  // * Tm pattern: variable

  [def-decl elab_var [-> Cxt Expr Name CheckTy [; Tm Vy]]]
  [def-impl elab_var [\ cxt expr_ref name check [do
    // 1. Try resolving as a local term variable
    [if-let [Some [; var_i var_ty]] [cxt . Cxt.tm_scope . Scope.resolve name] [do
      [on_inferred_tm cxt expr_ref [Tm.var var_i] var_ty check]
    ]]

    // 2. Try resolving as a def reference
    [if-let [Some uid] [cxt . Cxt.lookup_namespace name] [do
      [match [cxt . Cxt.db . Database.get_entry uid]
        [[Def def] [do
          [let ty [eval_ty [Cxt.eval cxt] [Env.nil] [def . XDef.ty]]]
          [on_inferred_tm cxt expr_ref [Tm.Def uid] ty check]
        ]]
        [_ [do
          [let msg [Foldable.mconcat [vec
            [Doc.of [type Void] "Term variable "]
            [Doc.of name . Doc.squotes]
            [Doc.of " is not referencing a def"]
          ]]]
          [make_and_mark_error_elab_tm cxt expr_ref check msg]
        ]]
      ]
    ]]

    // 3. Try resolving as known constant names
    [if-let [true] [== name "false"] [do
      [on_inferred_tm cxt expr_ref [Tm.Lit [Lit.Bool false]] [Vy.known KnownType.Bool] check]
    ]]
    [if-let [true] [== name "true"] [do
      [on_inferred_tm cxt expr_ref [Tm.Lit [Lit.Bool true]] [Vy.known KnownType.Bool] check]
    ]]
    [if-let [true] [== name ","] [do
      [on_inferred_tm cxt expr_ref [Tm.Tuple [Array.create ,]] Vy.unit check]
    ]]

    // 4. Unknown term variable
    [make_and_mark_error_elab_tm cxt expr_ref check [Foldable.mconcat [vec
      [Doc.of [type Void] "Unknown term variable: "]
      [Doc.of name . Doc.squotes]
    ]]]
  ]]]

  [def-decl tm_pat.var [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.var [
    PExpr.capture [PExpr.label "<variable>" PExpr.any_sym]
    . map [\ input cxt check [do
      [let= [; name_expr name] input]
      [elab_var cxt name_expr name check]
    ]]
  ]]

  // * Tm pattern: fluent / application

  [class-decl EApp]
  [class-enum EApp
    [member Tm [-> , [; Tm Vy]]]
    [member CallFn [; EApp Expr]]
    // (fn, expr ref, a function to elaborate the argument)
    [member ApplyTmArg [; EApp Expr [-> CheckTy [; Tm Vy]]]]
    [member ApplyTyArg [; EApp Expr]]
    // (fn, expr ref, field name)
    [member GetDictKey [; EApp Expr Name]]
  ]

  [def-decl elab_app [-> Cxt Expr EApp CheckTy [; Tm Vy]]]
  [def-impl elab_app [\ cxt in_expr_ref in_app in_check [do
    [let* [apply_holes [\ [params [Dict [forall Bool] [auto Bool]]] [expr_ref Expr] [mk_tm [-> , Tm]] [ty Vy] [do
      [let ty [force_ty_weak [Cxt.eval cxt] ty]]
      [match ty
        [[Arr [; [Auto] dom cod]] [where [params ./ auto]] [do
          // Fn wants auto
          // ... Produce an auto argument
          [let mk_res_tm [\ _ [do
            [let f [mk_tm ,]]
            [let x [match [resolve_instance cxt expr_ref dom]
              [[Some sol] [do
                sol
              ]]
              [[None] [do
                // Use a substitute for error-tolerance
                [let dom_doc [Cxt.render_vy cxt dom]]
                [Cxt.mark_expr_error cxt expr_ref [Doc.of "No solution for auto: " . <> dom_doc]]
                Tm.Error
              ]]
            ]]
            [Tm.app f x]
          ]]]
          [apply_holes params expr_ref mk_res_tm cod]
        ]]
        [[Forall [; kind cod]] [where [params ./ forall]] [do
          // Fn wants forall
          // ... Generate a type hole
          [let ty [Cxt.fresh_ty_meta cxt kind [MetaReason.new expr_ref [Doc.of "type argument hole"]]]]
          [let cod [eval_ty_closure [Cxt.eval cxt] cod ty]]
          [apply_holes params expr_ref mk_tm cod]
        ]]
        [_ [do
          // Done
          [; mk_tm ty]
        ]]
      ]
    ]]]]

    [let* [apply_arg [\ [expr_ref Expr] [mk_fn_tm [-> , Tm]] [fn_ty Vy] [elab_arg [-> CheckTy [; [-> , Tm] Vy]]] [do
      [let fn_ty [force_ty_weak [Cxt.eval cxt] fn_ty]]
      [match fn_ty
        [[Arr [; [Expl] dom cod]] [do
          // Check if type is nice
          [let mk_res_tm [\ _ [do
            [let f [mk_fn_tm ,]]

            // NOTE: MUST BE AFTER 'mkFnTm'. 'mkFnTm' may solve metas for 'dom'.
            [let= [; mk_arg_tm _] [elab_arg [CheckTy.Check dom]]]
            [let x [mk_arg_tm ,]]

            [Tm.app f x]
          ]]]
          [; mk_res_tm cod]
        ]]
        [_ [do
          // Infer for 'fn_ty'
          [let= [; mk_arg_tm arg_ty] [elab_arg CheckTy.Infer]]
          [let res_ty [Cxt.fresh_ty_meta cxt Kind.Star [MetaReason.new expr_ref [Doc.of "result type hole for inferred application"]]]]

          [let u_res [unify_ty_or_report cxt expr_ref fn_ty [Vy.Arr Icit.Expl arg_ty res_ty]]]
          [match u_res
            [[Fail] [do
              // Arg has bad type. Drop this arg for error-tolerance
              [; mk_fn_tm fn_ty]
            ]]
            [[Ok] [do
              [let mk_res_tm [\ _ [do
                [let f [mk_fn_tm ,]]
                [let x [mk_arg_tm ,]]
                [Tm.app f x]
              ]]]
              [; mk_res_tm res_ty]
            ]]
          ]
        ]]
      ]
    ]]]]

    [let* [check_app [type [-> EApp [; [-> , Tm] Vy]]] [\ input [[match input
      [[Tm infer] [do
        [let= [; tm ty] [infer ,]]
        [; [\ _ tm] ty]
      ]]
      [[ApplyTmArg [; fn expr_ref elab_arg_tm]] [do
        // FIXME: Bad expr ref
        [let= [; mk_fn_tm fn_ty] [check_app fn]]
        [let= [; mk_fn_tm fn_ty] [apply_holes [dict [forall true] [auto true]] expr_ref mk_fn_tm fn_ty]]
        [apply_arg expr_ref mk_fn_tm fn_ty [\ check [do
          [let= [; arg_tm arg_ty] [elab_arg_tm check]]
          [; [\ _ arg_tm] arg_ty]
        ]]]
      ]]
      [[CallFn [; app fn]] [do
        // FIXME: Bad expr ref
        [let expr_ref fn]
        [let= [; fn_tm fn_ty] [infer_tm cxt fn]]
        [let mk_fn_tm [\ _ fn_tm]]
        [let= [; mk_fn_tm fn_ty] [apply_holes [dict [forall true] [auto true]] expr_ref mk_fn_tm fn_ty]]
        [apply_arg expr_ref mk_fn_tm fn_ty [\ check [do
          [let= [; arg_tm arg_ty] [elab_app cxt expr_ref app check]]
          [; [\ _ arg_tm] arg_ty]
        ]]]
      ]]
      [[ApplyTyArg [; app ty_arg]] [do
        // FIXME: Bad expr ref
        [let expr_ref ty_arg]
        [let= [; mk_app_tm app_ty] [check_app app]]
        [let= [; mk_app_tm app_ty] [apply_holes [dict [forall false] [auto true]] expr_ref mk_app_tm app_ty]]

        [let app_ty [force_ty_weak [Cxt.eval cxt] app_ty]]
        [match app_ty
          [[Forall [; kind cod]] [do
            // Check if type arg is nice
            [let ty_arg [check_ty cxt ty_arg kind]]
            [let ty_arg [eval_ty [Cxt.eval cxt] [Env.of_ty_bounds [cxt . Cxt.ty_scope . Scope.lvl]] ty_arg]]
            [let cod [eval_ty_closure [Cxt.eval cxt] cod ty_arg]]
            [; mk_app_tm cod]
          ]]
          [_ [do
            [let= [; _ty_arg ty_arg_kind] [infer_ty cxt ty_arg]]

            [let cod [Cxt.fresh_ty_meta cxt Kind.Star [MetaReason.new ty_arg [Doc.of "result type hole"]]]]
            [let cod* [quote_ty [Cxt.eval cxt] [cxt . Cxt.ty_scope . Scope.lvl] cod]]
            [let cod* [TyClosure.new [Env.of_ty_bounds [cxt . Cxt.ty_scope . Scope.lvl]] cod*]]
            [let inferred_app_ty [Vy.Forall ty_arg_kind cod*]]

            // FIXME: This is a bad 'expr_ref' for 'unify_ty_or_report'
            [let u_result [unify_ty_or_report cxt ty_arg app_ty inferred_app_ty]]
            [match u_result
              [[Fail] [do
                // Drop this type arg for error-tolerance
                [; mk_app_tm app_ty]
              ]]
              [[Ok] [do
                [; mk_app_tm cod]
              ]]
            ]
          ]]
        ]
      ]]
      [[GetDictKey [; app name_expr_ref name]] [do
        // FIXME: Bad expr ref
        [let= [; mk_app_tm app_ty] [check_app app]]
        [let= [; mk_app_tm app_ty] [apply_holes [dict [forall true] [auto true]] in_expr_ref mk_app_tm app_ty]]

        [let on_cannot_get_field [\ _ [do
          [TODO "on_cannot_get_field"]
        ]]]

        [let app_ty [force_ty_weak [Cxt.eval cxt] app_ty]]

        [else-let [Dict d] app_ty
          [on_cannot_get_field ,]]

        [else-let [Some field_ty] [d . StringMap.get? name]
          [on_cannot_get_field ,]]

        [let mk_res_tm [\ _ [do
          [let object [mk_app_tm ,]]
          [Tm.GetDictKey object name]
        ]]]

        [; mk_res_tm field_ty]
      ]]
    ]]]]]

    [let= [; mk_app_tm app_ty] [check_app in_app]]
    [let= [; mk_app_tm app_ty] [apply_holes [dict [forall true] [auto true]] in_expr_ref mk_app_tm app_ty]]
    [match in_check
      [[Check exp_ret_ty] [do
        // We immediately unify the final application type with the expected
        // return type. This way, we could infer types of the arguments much
        // better
        [let u_result [unify_ty_or_report cxt in_expr_ref app_ty exp_ret_ty]]
        [match u_result
          [[Fail] [do
            // If the application's type is bad, drop the whole thing for
            // error-tolerance.
            [; Tm.Error exp_ret_ty]
          ]]
          [[Ok] [do
            [let app_tm [mk_app_tm ,]]
            [; app_tm exp_ret_ty]
          ]]
        ]
      ]]
      [[Infer] [do
        // Otherwise, use whatever we have for 'app_ty'
        [let app_tm [mk_app_tm ,]]
        [; app_tm app_ty]
      ]]
    ]
  ]]]

  [def-decl tm_pat.fluent.modifier_pat [PList [-> Cxt EApp EApp]]]
  [def-impl tm_pat.fluent.modifier_pat [do
    [Foldable.asum [vec
      [[PList.one_of [PExpr.sym "."] . *> [PList.one_of PExpr.any_expr]]
        . map [\ [fn Expr] [cxt Cxt] [app EApp] [EApp.CallFn app fn]]]
      [[PList.one_of [PExpr.sym "./"] . *> [PList.one_of [PExpr.label "<field-name>" [PExpr.capture PExpr.any_sym]]]]
        . map [\ [in_name [; Expr Name]] [cxt Cxt] [app EApp] [do
          [let= [; name_expr_ref name] in_name]
          [EApp.GetDictKey app name_expr_ref name]
        ]]
      ]
      [[PList.one_of [PExpr.list [PList.one_of [PExpr.sym "type"] . *> [PList.one_of PExpr.any_expr]]]]
        . map [\ [fn Expr] [cxt Cxt] [app EApp] [EApp.ApplyTyArg app fn]]]
      [[PList.one_of PExpr.any_expr]
        . map [\ [arg Expr] [cxt Cxt] [app EApp] [EApp.ApplyTmArg app arg [elab_tm cxt arg]]]]
    ]]
  ]]

  [def-decl tm_pat.fluent [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.fluent
    [PExpr.capture [PExpr.list [PList.some_of [PExpr.label "<term>" PExpr.any_expr]]] . map [\ input cxt check [do
      [let= [; self entries] input]
      [let head_expr [entries . Array.pop_first]]
      [let tail_exprs [entries]]

      [let modifiers [PList.many tm_pat.fluent.modifier_pat . PList.match self entries
        . Either.expect "tm_pat.fluent.modifier_pat should never fail"]]
      [let= app [modifiers
        . Array.iter
        . Iter.reduce
            [EApp.Tm [\ _ [infer_tm cxt head_expr]]]
            [\ state modifier [modifier cxt state]]
      ]]
      [elab_app cxt self app check]
    ]]]
  ]

  // * Tm pattern: lambda

  [def-decl tm_pat.lambda.elab_arr_param [
    ->
    [; Expr Icit Name [Maybe Expr]]
    Cxt CheckTy
    [-> Cxt CheckTy [; Tm Vy]]
    [; Tm Vy]
  ]]
  [def-impl tm_pat.lambda.elab_arr_param [\ input cxt check check_rest [do
    [let= [; self icit name maybe_ty_expr] input]
    [let check_rest_with [\ [name Name] [dom_ty Vy] [check CheckTy] [do
      [let cxt [cxt . Cxt.extend_tm [Scope.one name dom_ty]]]
      [let cxt [if [icit . == Icit.Auto]
        [cxt . Cxt.extend_tm_bound_auto [cxt . Cxt.tm_scope . Scope.lvl . I32.- 1]]
        cxt
      ]]
      [check_rest cxt check]
    ]]]

    [let check [force_check_ty_weak cxt check]]
    [match check
      [[Check [Arr [; exp_icit exp_dom exp_cod]]] [where [== icit exp_icit]] [do
        // If the user type-annotated the parameter, it must match the expected
        // parameter type.
        [match maybe_ty_expr
          [[None] ,]
          [[Some ty_expr] [do
            [let got_dom [check_ty cxt ty_expr Kind.Star]]
            [let got_dom [eval_ty [Cxt.eval cxt] [Env.of_ty_bounds [cxt . Cxt.ty_scope . Scope.lvl]] got_dom]]
            [let _ [unify_ty_or_report cxt ty_expr got_dom exp_dom]]
            ,
          ]]
        ]

        [let= [; rest_tm _] [check_rest_with name exp_dom [CheckTy.Check exp_cod]]]
        [on_checked_tm [Tm.Lam [mark_tail_calls rest_tm]] check]
      ]]
      [_ [do
        // If the user type-annotated the parameter, use it, otherwise we
        // generate a type hole placeholder.
        [let dom [match maybe_ty_expr
          [[None] [do
            [Cxt.fresh_ty_meta cxt Kind.Star [MetaReason.new self [Doc.of "type placeholder"]]]
          ]]
          [[Some ty_expr] [do
            [let got_dom [check_ty cxt ty_expr Kind.Star]]
            [let got_dom [eval_ty [Cxt.eval cxt] [Env.of_ty_bounds [cxt . Cxt.ty_scope . Scope.lvl]] got_dom]]
            got_dom
          ]]
        ]]

        [let= [; rest_tm rest_ty] [check_rest_with name dom CheckTy.Infer]]
        [on_inferred_tm cxt self [Tm.Lam [mark_tail_calls rest_tm]] [Vy.Arr icit dom rest_ty] check]
      ]]
    ]
  ]]]

  [def-decl tm_pat.lambda.param_name_only_pat
    [PExpr [-> Cxt CheckTy [-> Cxt CheckTy [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.lambda.param_name_only_pat [
    [\ [input [; Expr Name]] cxt check_rest check [do
      [let= [; self name] input]
      [tm_pat.lambda.elab_arr_param [; self Icit.Expl name [None]] cxt check_rest check]
    ]]
    . <$> [PExpr.label "<name>" [PExpr.capture PExpr.any_sym]]
  ]]

  [def-decl tm_pat.lambda.annotated_param_pat
    [PExpr [-> Cxt CheckTy [-> Cxt CheckTy [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.lambda.annotated_param_pat [
    [PExpr.captures [PExpr.list [
      [\ is_auto name maybe_ty_expr self cxt check_rest check [do
        [let icit [if is_auto Icit.Auto Icit.Expl]]
        [tm_pat.lambda.elab_arr_param
          [; self icit name maybe_ty_expr] cxt check_rest check]
      ]]
      . <$> [PList.modifier [PExpr.sym "auto"]]
      . <*> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
      . <*> [PList.optional_of [PExpr.label "<type>" PExpr.any_expr]]
    ]]]
  ]]

  [def-decl tm_pat.lambda.forall_pat
    [PExpr [-> Cxt CheckTy [-> Cxt CheckTy [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.lambda.forall_pat [PExpr.block "type" [
    [\ [name Name] [maybe_kind_expr [Maybe Expr]] [block PExpr.Block] [cxt Cxt] [check CheckTy] [check_rest [-> Cxt CheckTy [; Tm Vy]]] [do
      [let self [block . PExpr.Block.self]]

      // If the user provided a kind annotation on the forall-bound type
      // variable, use theirs, otherwise generate a kind hole.
      [let got_kind [match maybe_kind_expr
        [[None] [do
          [Cxt.fresh_kind_meta cxt [MetaReason.new self [Doc.of "kind placeholder"]]]
        ]]
        [[Some kind_expr] [do
          [elab_kind cxt kind_expr]
        ]]
      ]]

      [let check [force_check_ty_weak cxt check]]
      [match check
        [[Check [Forall [; exp_kind exp_cod]]] [do
          [let _ [unify_kind_or_report cxt self got_kind exp_kind]]
          [let exp_cod
            [eval_ty_closure [Cxt.eval cxt] exp_cod [Vy.bound [cxt . Cxt.ty_scope . Scope.lvl]]]]
          [let= [; rest_tm _]
            [check_rest [cxt . Cxt.extend_ty [Scope.one name exp_kind]] [CheckTy.Check exp_cod]]]
          [on_checked_tm rest_tm check]
        ]]
        [_ [do
          [let= [; rest_tm rest_ty]
            [check_rest [cxt . Cxt.extend_ty [Scope.one name got_kind]] CheckTy.Infer]]
          [let rest_ty
            [quote_ty [Cxt.eval cxt] [cxt . Cxt.ty_scope . Scope.lvl . I32.+ 1] rest_ty]]
          [let got_ty
            [Vy.Forall got_kind [TyClosure.new [Env.of_ty_bounds [cxt . Cxt.ty_scope . Scope.lvl]] rest_ty]]]
          [on_inferred_tm cxt self rest_tm got_ty check]
        ]]
      ]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
    . <*> [PList.optional_of [PExpr.label "<kind>" PExpr.any_expr]]
  ]]]

  [def-decl tm_pat.lambda.param_pat
    [PExpr [-> Cxt CheckTy [-> Cxt CheckTy [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.lambda.param_pat [Foldable.mconcat [vec
    tm_pat.lambda.forall_pat
    tm_pat.lambda.param_name_only_pat
    tm_pat.lambda.annotated_param_pat
  ]]]

  [def-decl tm_pat.lambda [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.lambda [PExpr.block_ "\\" [
    [\ [in_entries [Array Expr]] [in_cxt Cxt] [in_check CheckTy] [do
      [let* [go [\ [i I32] [cxt Cxt] [check CheckTy] [do
        [let entry [in_entries . Array.at i]]

        [if-let [true] [in_entries . Array.is_last_ix i] [do
          // Last entry must be the lambda's body
          [elab_tm cxt entry check]
        ]]

        // Entries before the last are parameters
        [match [match_or_report cxt entry tm_pat.lambda.param_pat]
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

  // * Tm pattern: [if <cond> <on-true> <on-false>]

  [def-decl tm_pat.if [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.if [PExpr.block "if" [
    [\ [cond_expr Expr] [on_true_expr Expr] [on_false_expr Expr] [self PExpr.Block] [cxt Cxt] [check CheckTy] [do
      [let cond [check_tm cxt cond_expr Vy.bool]]
      [match check
        [[Check exp_ty] [do
          [let on_true [check_tm cxt on_true_expr exp_ty]]
          [let on_false [check_tm cxt on_false_expr exp_ty]]
          [; [Tm.If cond on_true on_false] exp_ty]
        ]]
        [[Infer] [do
          // The true branch determines the return type of the expression.
          [let= [; on_true got_ty] [infer_tm cxt on_true_expr]]
          [let on_false [check_tm cxt on_false_expr got_ty]]
          [; [Tm.If cond on_true on_false] got_ty]
        ]
      ]
    ]]]
    . <$> [PList.one_of [PExpr.label "<cond>" PExpr.any_expr]]
    . <*> [PList.one_of [PExpr.label "<on-true>" PExpr.any_expr]]
    . <*> [PList.one_of [PExpr.label "<on-false>" PExpr.any_expr]]
  ]]]

  // * Tm pattern: [do <stmt>+]

  [def-decl tm_pat.do.expr_stmt_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.expr_stmt_pat [
    PExpr.any_expr . map [\ expr cxt check check_rest [do
      [let let_ty [Vy.unit]]
      [let let_cxt [cxt . Cxt.extend_tm [Scope.one "" let_ty]]]
      [let let_val [check_tm let_cxt expr let_ty]]
      [let= [; rest_tm rest_ty] [check_rest let_cxt]]
      [; [Tm.Let [Array.one let_val] rest_tm] rest_ty]
    ]]
  ]]

  [class-decl tm_pat.do.elab_let.Entry]
  [class-struct tm_pat.do.elab_let.Entry
    [field expr_ref Expr]
    [field name Name]
    [field value Expr]
    [field value_type [Maybe Expr]]
  ]

  [def-decl tm_pat.do.elab_let
    [-> Bool [Array tm_pat.do.elab_let.Entry] Cxt [-> Cxt [; Tm Vy]] [; Tm Vy]]]
  [def-impl tm_pat.do.elab_let [\ is_let_recursive? let_entries cxt check_rest [do
    [let num_entries [let_entries . Array.length]]

    [let let_types [Array.create ,]]
    [let let_scope [Array.create ,]]

    [loop<n num_entries [\ i [do
      [let= [mk [; expr_ref name _value value_type]] [let_entries . Array.at i]]
      [let value_type [match value_type
        [[None] [Cxt.fresh_ty_meta cxt Kind.Star [MetaReason.new expr_ref [Doc.of "let binding type hole"]]]]
        [[Some value_type] [do
          [let value_type [check_ty cxt value_type Kind.Star]]
          [eval_ty [Cxt.eval cxt] [Env.of_ty_bounds [cxt . Cxt.ty_scope . Scope.lvl]] value_type]
        ]]
      ]]
      [let_types . Array.append value_type]
      [let_scope . Array.append [; name value_type]]
      continue
    ]]]
    [let let_scope [let_scope . Array.iter . Scope.from_iter_reversed]]

    [let let_val_cxt [cxt . Cxt.extend_tm [let_scope 
      . apply_unless is_let_recursive? [Scope.nullify_resolve]]]]
    [let let_vals [Array.create ,]]
    [loop<n num_entries [\ i [do
      [let= [mk [; _expr_ref _name value _value_type]] [let_entries . Array.at i]]
      [let let_type [let_types . Array.at i]]
      [let let_val [check_tm let_val_cxt value let_type]]
      [let_vals . Array.append let_val]
      continue
    ]]]

    [let rest_cxt [cxt . Cxt.extend_tm let_scope]]
    [let= [; rest_tm rest_ty] [check_rest rest_cxt]]

    [; [Tm.Let let_vals rest_tm] rest_ty]
  ]]]

  [def-decl tm_pat.do.let_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.let_pat [PExpr.block "let" [
    [\ [name Name] [value Expr] block cxt check check_rest [do
      [let self [block . PExpr.Block.self]]
      [let let_entry [tm_pat.do.elab_let.Entry.new self name value [None]]]
      [tm_pat.do.elab_let false [Array.one let_entry] cxt check_rest]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
    . <*> [PList.one_of [PExpr.label "<value>" PExpr.any_expr]]
  ]]]

  [def-decl tm_pat.do.let*_pat.entry_pat [PExpr tm_pat.do.elab_let.Entry]]
  [def-impl tm_pat.do.let*_pat.entry_pat [
    PExpr.captures [PExpr.list [
      [\ [name Name] [value_type [Maybe Expr]] [value Expr] self [do
        [tm_pat.do.elab_let.Entry.new self name value value_type]
      ]]
      . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
      . <*> [PList.optional_of [PExpr.block_ "type" [PList.one_of [PExpr.label "<type>" PExpr.any_expr]]]]
      . <*> [PList.one_of [PExpr.label "<value>" PExpr.any_expr]]
    ]]
  ]]

  [def-decl tm_pat.do.let*_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.let*_pat [PExpr.block_ "let*" [
    [ \ [entry_exprs [Array Expr]] [cxt Cxt] [check CheckTy] check_rest [do
      [let let_entries [Array.create ,]]
      [entry_exprs . Array.foreach [\ entry_expr [do
        [let result [match_or_report cxt entry_expr tm_pat.do.let*_pat.entry_pat]]
        [result . Maybe.when_some [\ let_entry [do
          [let_entries . Array.append let_entry]
        ]]]
        continue
      ]]]
      [tm_pat.do.elab_let true let_entries cxt check_rest]
    ]]
    . <$> PList.rest
  ]]]

  [def-decl tm_pat.do.if/else-let_pat [-> String Bool [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]]
  [def-impl tm_pat.do.if/else-let_pat [\ leader is-if-let= [PExpr.block_ leader [
    [\
      [pat_expr Expr]
      [maybe_guard_expr [Maybe Expr]]
      [scrut_expr Expr]
      [branch_expr Expr]
      [cxt Cxt]
      [check CheckTy]
      [check_rest [-> Cxt [; Tm Vy]]] [do
        [let= [; scrut_tm scrut_ty] [infer_tm cxt scrut_expr]]
        [let= [; pat pat_scope] [pmatch.check_pattern_expr cxt scrut_ty pat_expr]]

        [let pat_cxt [cxt . Cxt.extend_tm pat_scope]]
        [let rest_cxt [if is-if-let= cxt pat_cxt]]
        [let branch_cxt [if is-if-let= pat_cxt cxt]]
            
        [let= [; rest_tm rest_ty] [check_rest rest_cxt]]

        [let branch_tm [check_tm branch_cxt branch_expr rest_ty]]
        [let guard_tm [maybe_guard_expr . Maybe.map [\ guard_expr [check_tm pat_cxt guard_expr Vy.bool]]]]

        [let out_tm [pmatch.compile_user_clauses cxt
          [pmatch.compile_user_clauses.Params.new* [dict
            [scrut_expr_ref scrut_expr]
            [scrut_tm scrut_tm]
            [scrut_ty scrut_ty]
            [user_clauses [Array.two
              [PatUserClause.new* [dict
                [pat pat]
                [pat_scope pat_scope]
                [guard_tm guard_tm]
                [body_tm [if is-if-let= branch_tm rest_tm]]
              ]]
              [PatUserClause.new* [dict
                [pat [Pat.new pat_expr PatData.Ignored]]
                [pat_scope [Scope.nil]]
                [guard_tm [None]]
                [body_tm [if is-if-let= rest_tm branch_tm]]
              ]]
            ]]
          ]]
        ]]
        [; out_tm rest_ty]
      ]
    ]
    . <$> [PList.one_of [PExpr.label "<pat>" PExpr.any_expr]]
    . <*> [PList.optional_of [PExpr.block_ "where" [PList.one_of [PExpr.label "<guard>" PExpr.any_expr]]]]
    . <*> [PList.one_of [PExpr.label "<scrutinee>" PExpr.any_expr]]
    . <*> [PList.one_of [PExpr.label "<on-match>" PExpr.any_expr]]
  ]]]]

  [def-decl tm_pat.do.if-let_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.if-let_pat [tm_pat.do.if/else-let_pat "if-let" true]]

  [def-decl tm_pat.do.else-let_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.else-let_pat [tm_pat.do.if/else-let_pat "else-let" false]]

  [def-decl tm_pat.do.if/else-let=_pat [-> String Bool [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]]
  [def-impl tm_pat.do.if/else-let=_pat [\ leader is-if-let= [PExpr.block_ leader [
    [\
      [primary_pat_expr Expr]
      [primary_guard_expr [Maybe Expr]]
      [scrut_expr Expr]
      [secondary_clause_exprs [Array Expr]]
      [cxt Cxt]
      [check CheckTy]
      [check_rest [-> Cxt [; Tm Vy]]] [do
        [let= [; scrut_tm scrut_ty] [infer_tm cxt scrut_expr]]

        [let= [; primary_pat primary_pat_scope] [pmatch.check_pattern_expr cxt scrut_ty primary_pat_expr]]
        [let primary_cxt [cxt . Cxt.extend_tm primary_pat_scope]]
        [let primary_guard_tm [primary_guard_expr . Maybe.map [\ guard_expr [check_tm cxt guard_expr Vy.bool]]]]

        [let rest_cxt [cxt . Cxt.extend_tm primary_pat_scope]]
        [let= [; rest_tm rest_ty] [check_rest rest_cxt]]

        [let user_clauses [Array.create ,]]

        // Primary clause
        //
        // (To be added later)
        [let primary_user_clause [
          PatUserClause.new* [dict
            [pat primary_pat]
            [pat_scope primary_pat_scope]
            [guard_tm primary_guard_tm]
            [body_tm rest_tm]
          ]
        ]]

        // Add secondary clauses
        [secondary_clause_exprs . Array.foreach [\ clause_expr [do
          [let= [Some [mk [; pat_expr maybe_guard_expr body_expr]]]
            [match_or_report cxt clause_expr [PExpr.list tm_pat.common_user_clause_pat]]
            [[None] [do
              // Skip bad patterns for error-tolerance
              continue
            ]]
          ]

          [let= [; pat pat_scope] [pmatch.check_pattern_expr cxt scrut_ty pat_expr]]
          [let secondary_cxt [cxt . Cxt.extend_tm pat_scope]]
          [let maybe_guard_tm [maybe_guard_expr
            . Maybe.map [\ guard_expr [check_tm secondary_cxt guard_expr Vy.bool]]]]
          [let body_tm [check_tm secondary_cxt body_expr rest_ty]]

          [user_clauses . Array.append [PatUserClause.new* [dict
            [pat pat]
            [pat_scope pat_scope]
            [guard_tm maybe_guard_tm]
            [body_tm body_tm]
          ]]]

          continue
        ]]]

        // The way we place the primary user clause determines this match-let's
        // behavior
        [if is-if-let=
          [user_clauses . Array.prepend primary_user_clause]
          [user_clauses . Array.append primary_user_clause]
        ]

        [let out_tm [pmatch.compile_user_clauses cxt
          [pmatch.compile_user_clauses.Params.new* [dict
            [scrut_expr_ref scrut_expr]
            [scrut_tm scrut_tm]
            [scrut_ty scrut_ty]
            [user_clauses user_clauses]
          ]]
        ]]
        [; out_tm rest_ty]
      ]
    ]
    . <$> [PList.one_of [PExpr.label "<pat>" PExpr.any_expr]]
    . <*> [PList.optional_of [PExpr.block_ "where" [PList.one_of [PExpr.label "<guard>" PExpr.any_expr]]]]
    . <*> [PList.one_of [PExpr.label "<scrutinee>" PExpr.any_expr]]
    . <*> [PList.many_of [PExpr.label "<clause>" PExpr.any_expr]]
  ]]]]

  [def-decl tm_pat.do.let=_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.let=_pat [tm_pat.do.if/else-let=_pat "let=" true]]

  [def-decl tm_pat.do.else-let=_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.else-let=_pat [tm_pat.do.if/else-let=_pat "else-let=" false]]

  // Like the Haskell bind syntax '[try <name> <action>]' = 'do <name> <-
  // <action>; ...'
  //
  // TODO: Add the '?' try operator from Rust / Idris2's '!' bang-notation /
  // Lean4's inline '<-' are very flexible, but implementing them is slightly
  // annoying, but brings a lot of conveniences like direct 'let'/pattern match
  // usage.
  [def-decl tm_pat.do.try_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.try_pat [PExpr.block "try" [
    [\ [name Name] [ma_expr Expr] [block PExpr.Block] [cxt Cxt] [check CheckTy] [check_rest [-> Cxt [; Tm Vy]]] [do
      [let self [block . PExpr.Block.self]]

      // Hand-craft an 'EApp' to simulate "[then maExpr [\ x rest]]"
      [let app [EApp.Tm
        [\ _ [elab_var cxt self "then" CheckTy.Infer]]
      ]]
      [let app [EApp.ApplyTmArg app
        ma_expr
        [elab_tm cxt ma_expr]
      ]]
      [let app [EApp.ApplyTmArg app
        self
        [\ check [do
          [let check [force_check_ty_weak cxt check]]
          [match check
            [[Check [as exp_ty [Arr [; [Expl] dom cod]]]] [do
              // Check when the type is nice
              [let rest_cxt [cxt . Cxt.extend_tm [Scope.one name dom]]]
              [let= [; rest_tm rest_ty] [check_rest rest_cxt]]

              // 'cod' should really match that of the 'check' of 'elab_tm', but
              // we will unify with 'cod' anyway to detect user mistyped
              // 'then' functions.
              [let u_res [unify_ty_or_report cxt self rest_ty cod]]
              [match u_res
                [[Fail] [do
                  // Drop the whole thing for error-tolerance.
                  [; Tm.Error exp_ty]
                ]]
                [[Ok] [do
                  [; [Tm.Lam rest_tm] exp_ty]
                ]]
              ]
            ]]
            [_ [do
              // Infer for 'check'
              [let dom [Cxt.fresh_ty_meta cxt Kind.Star [MetaReason.new self [Doc.of "[try ...] 'dom' type"]]]]

              [let rest_cxt [cxt . Cxt.extend_tm [Scope.one name dom]]]
              [let= [; rest_tm rest_ty] [check_rest rest_cxt]]

              [let inferred_ty [Vy.Arr Icit.Expl dom rest_ty]]
              [on_inferred_tm cxt self [Tm.Lam rest_tm] inferred_ty check]
            ]]
          ]
        ]]
      ]]

      [elab_app cxt self app check]
    ]]
    . <$> [PList.one_of [PExpr.any_sym]]
    . <*> [PList.one_of [PExpr.any_expr]]
  ]]]

  [def-decl tm_pat.do.stmt_pat [PExpr [-> Cxt CheckTy [-> Cxt [; Tm Vy]] [; Tm Vy]]]]
  [def-impl tm_pat.do.stmt_pat [Foldable.mconcat [vec
    tm_pat.do.let_pat
    tm_pat.do.let*_pat
    tm_pat.do.let=_pat
    tm_pat.do.else-let=_pat
    tm_pat.do.if-let_pat
    tm_pat.do.else-let_pat
    tm_pat.do.try_pat
    tm_pat.do.expr_stmt_pat // Must be last since it uses 'PExpr.any_expr'
  ]]]

  [def-decl tm_pat.do [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.do [PExpr.block "do" [
    [\ [stmt_exprs [Array Expr]] [block PExpr.Block] in_cxt in_check [do
      [let last_expr [stmt_exprs . Array.pop_last]]

      [let stmt_exprs_iter [stmt_exprs . Array.iter]]
      [let* [check_rest [\ [cxt Cxt] [do
        [match [stmt_exprs_iter . Iter.next?]
          [[None] [do
            // Process the last statement (as an expression)
            [elab_tm cxt last_expr in_check]
          ]]
          [[Some stmt_expr] [do
            // Process statements before the last statement
            [else-let [Some elab_stmt] [match_or_report cxt stmt_expr tm_pat.do.stmt_pat] [do
              // Skip bad statements for error-tolerance
              [check_rest cxt]
            ]]
            [elab_stmt cxt in_check check_rest]
          ]]
        ]
      ]]]]
      [check_rest in_cxt]
    ]]
    . <$> [PList.some_of [PExpr.label "<stmt>" PExpr.any_expr]]
  ]]]

  [def-decl tm_pat.lit_string [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.lit_string [PExpr.capture PExpr.any_str . map [\ input cxt check [do
    [let= [; self str] input]
    [on_inferred_tm cxt self [Tm.Lit [Lit.String str]] [Vy.string] check]
  ]]]]

  [def-decl tm_pat.lit_char [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.lit_char [PExpr.capture PExpr.any_char . map [\ input cxt check [do
    [let= [; self ch] input]
    [on_inferred_tm cxt self [Tm.Lit [Lit.Char ch]] [Vy.char] check]
  ]]]]

  [def-decl tm_pat.lit_int [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.lit_int [PExpr.capture PExpr.any_int . map [\ input cxt check [do
    [let= [; self i] input]
    [on_inferred_tm cxt self [Tm.Lit [Lit.I32 [Int.clamp_to_i32 i]]] [Vy.i32] check]
  ]]]]

  [def-decl tm_pat.match [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.match [PExpr.block "match" [
    [\ [scrut_expr Expr] [clause_exprs [Array Expr]] [block PExpr.Block] [cxt Cxt] [check CheckTy] [do
      [let self [block . PExpr.Block.self]]
      [let= [; scrut_tm scrut_ty] [infer_tm cxt scrut_expr]]

      // The type of the whole match expression
      [let whole_ty [match check
        [[Check t] t]
        [[Infer] [do
          [Cxt.fresh_ty_meta cxt Kind.Star [MetaReason.new self [Doc.of "match return type hole"]]]
        ]]
      ]]

      [let user_clauses [Array.create [type PatUserClause] ,]]
      [clause_exprs . Array.foreach [\ clause_expr [do
        // Parse 'clause_expr' and drop it for error-tolerance if the syntax is
        // malformed.
        [let= [Some [mk [; pat_expr maybe_guard_expr body_expr]]]
          [match_or_report cxt clause_expr [PExpr.list tm_pat.common_user_clause_pat]]
          [[None] continue]
        ]

        [let= [; pat pat_scope] [pmatch.check_pattern_expr cxt scrut_ty pat_expr]]
        [let pat_cxt [cxt . Cxt.extend_tm pat_scope]]

        [let body_tm [check_tm pat_cxt body_expr whole_ty]]
        [let maybe_guard_tm [maybe_guard_expr . Maybe.map [\ guard_expr [do
          [check_tm pat_cxt guard_expr Vy.bool]
        ]]]]

        [user_clauses . Array.append [PatUserClause.new* [dict
          [pat pat]
          [pat_scope pat_scope]
          [body_tm body_tm]
          [guard_tm maybe_guard_tm]
        ]]]
        continue
      ]]]

      [let whole_tm [pmatch.compile_user_clauses cxt [pmatch.compile_user_clauses.Params.new* [dict
        [scrut_expr_ref scrut_expr]
        [scrut_ty scrut_ty]
        [scrut_tm scrut_tm]
        [user_clauses user_clauses]
      ]]]]
      [; whole_tm whole_ty]
    ]]
    . <$> [PList.one_of [PExpr.label "<scrutinee>" PExpr.any_expr]]
    . <*> [PList.rest]
  ]]]

  [def-decl tm_pat.dict [PExpr [-> Cxt CheckTy [; Tm Vy]]]]
  [def-impl tm_pat.dict [PExpr.block "dict" [
    [\ [in_entries [Array Expr]] [block PExpr.Block] [cxt Cxt] [check CheckTy] [do
      [let self [block . PExpr.Block.self]]

      [let entry_pat [PExpr.list [
        [Pair.new]
        . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
        . <*> [PList.one_of [PExpr.label "<value>" PExpr.any_expr]]
      ]]]

      [let check [force_check_ty_weak cxt check]]
      // Given a field name, return the expected type of the field's value if
      // possible.
      [let get_field_ty_check [match check
        [[Check [Dict field_tys]] [\ [name Name] [do
          [match [field_tys . StringMap.get? name]
            [[None] CheckTy.Infer]
            [[Some field_ty] [CheckTy.Check field_ty]]
          ]
        ]]]
        [_ [\ [name Name]
          CheckTy.Infer
        ]]
      ]]

      [let got_field_tms [Array.create ,]]
      [let got_field_tys [StringMutMap.create ,]]
      [in_entries . Array.foreach [\ in_entry [do
        [let result [match_or_report cxt in_entry entry_pat]]

        [else-let [Some [; name value]] result
          continue]

        [let= [; value value_ty] [elab_tm cxt value [get_field_ty_check name]]]
        [got_field_tms . Array.append [; name value]]
        [got_field_tys . StringMutMap.set name value_ty]
        continue
      ]]]

      [let got_ty [Vy.Dict [got_field_tys . StringMutMap.freeze]]]
      [let got_tm [Tm.Dict got_field_tms]]
      [on_inferred_tm cxt self got_tm got_ty check]
    ]]
    . <$> [PList.many_of [PExpr.label "<entry>" PExpr.any_expr]]
  ]]]

  [def-impl tm_pat [Foldable.mconcat [vec
    // Block patterns
    tm_pat.lambda
    tm_pat.tuple
    tm_pat.vec
    tm_pat.if
    tm_pat.do
    tm_pat.match
    tm_pat.dict

    tm_pat.fluent // Must be after anything that consumes an s-list expression
    tm_pat.lit_string
    tm_pat.lit_char
    tm_pat.lit_int
    tm_pat.var
  ]]]
]
