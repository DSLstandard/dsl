[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/cxt_utils.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/pretty_kind.dsl"]
  [import "dsl/pretty_ty.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/unify_kind.dsl"]
  [import "dsl/unify_ty.dsl"]
  [import "wlp.dsl"]

  [def-decl pub unify_kind_or_report [-> Cxt Expr Kind Kind UnifyKindResult]]
  [def-impl unify_kind_or_report [\ cxt self got_kind exp_kind [do
    [match [unify_kind cxt got_kind exp_kind]
      [[Ok] [do
        [UnifyKindResult.Ok]
      ]]
      [[Fail] [do
        [let msg [
          [Doc.of [type Void] "Has kind: "]
          . <> [Cxt.render_kind cxt got_kind]
          . <> [Doc.line]
          . <> [Doc.of "Should be: "]
          . <> [Cxt.render_kind cxt exp_kind]
        ]]
        [Cxt.mark_expr_error cxt self msg]
        [UnifyKindResult.Fail]
      ]]
    ]
  ]]]

  [def-decl pub unify_ty_or_report [-> Cxt Expr Vy Vy UnifyTyResult]]
  [def-impl unify_ty_or_report [\ cxt self got_ty exp_ty [
    [match [unify_ty cxt got_ty exp_ty]
      [[Ok] [do
        [UnifyTyResult.Ok]
      ]]
      [[Fail] [do
        [let name_env [cxt . Cxt.ty_scope . Scope.name_env]]
        [let msg [
          [Doc.of [type Void] "Has type: "]
          . <> [Cxt.render_vy cxt got_ty]
          . <> [Doc.line]
          . <> [Doc.of "Should be: "]
          . <> [Cxt.render_vy cxt exp_ty]
        ]]
        [Cxt.mark_expr_error cxt self msg]
        [UnifyTyResult.Fail]
      ]]
    ]
  ]]]
]