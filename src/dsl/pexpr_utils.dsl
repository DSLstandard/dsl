[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/pexpr.dsl"]
  [import "wlp.dsl"]

  [def-decl pub match_or_report [-> [type A] Cxt Expr [PExpr A] [Maybe A]]]
  [def-impl match_or_report [\ [type A] cxt expr pat [do
    [match [pat . PExpr.match expr]
      [[L err] [do
        [err . PError.to_array . Array.foreach [\ entry [do
          [cxt . Cxt.mark_expr_error
            [entry . PErrorEntry.expr]
            [entry . PErrorEntry.desc]
          ]
          continue
        ]]]
        [None]
      ]]
      [[R a] [do
        [Some a]
      ]]
    ]
  ]]]
]