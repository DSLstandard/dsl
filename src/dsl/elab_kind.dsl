[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/cxt_utils.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/pexpr.dsl"]
  [import "dsl/pexpr_utils.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "dsl/unify_kind.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "wlp.dsl"]

  [def-decl kind_pat [PExpr [-> Cxt Kind]]]

  [def-decl make_and_mark_error_kind [-> Cxt Expr [Doc Void] Kind]]
  [def-impl make_and_mark_error_kind [\ cxt expr msg [do
    [cxt . Cxt.mark_expr_error expr msg]
    [Cxt.fresh_kind_meta cxt [MetaReason.new expr msg]]
  ]]]

  [def-decl pub elab_kind [-> Cxt Expr Kind]]
  [def-impl elab_kind [\ cxt self [do
    [let result [match_or_report cxt self kind_pat]]
    [match result
      [[None] [make_and_mark_error_kind cxt self [Doc.of "Bad kind expression"]]]
      [[Some elab] [elab cxt]]
    ]
  ]]]

  [def-decl kind_pat.arrow [PExpr [-> Cxt Kind]]]
  [def-impl kind_pat.arrow [PExpr.block_ "->" [
    [\ entries cxt [do
      [let cod [entries . Array.pop_last]]

      [let doms [entries . Array.map [elab_kind cxt]]]
      [let cod [elab_kind cxt cod]]

      [Kind.fold_arr [doms . Array.iter] cod]
    ]]
    . <$> [PList.some_of [PExpr.label "<kind>" PExpr.any_expr]]
  ]]]

  [def-decl kind_pat.star [PExpr [-> Cxt Kind]]]
  [def-impl kind_pat.star [PExpr.sym "*" . $> [\ cxt [do
    [Kind.Star]
  ]]]]

  [def-impl kind_pat
    [Foldable.mconcat [vec
      kind_pat.arrow
      kind_pat.star
    ]]
  ]
]