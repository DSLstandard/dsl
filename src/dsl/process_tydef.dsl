[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/elab_kind.dsl"]
  [import "dsl/pexpr.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/elab_ty.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/types.dsl"]

  [def-decl pub pat_ty_def_decl [PExpr [-> Cxt ,]]]
  [def-impl pat_ty_def_decl [PExpr.block_ "tydef" [
    [\ [name Name] [kind_expr Expr] [ty_expr Expr] [cxt Cxt] [do
      [let kind [elab_kind cxt kind_expr]]
      [let ty [check_ty cxt ty_expr kind]]

      [let uid [cxt . Cxt.db . Database.fresh_uid]]
      [cxt . Cxt.db . Database.insert_entry
        [XEntry.TyDef [XTyDef.new* [dict
          [uid uid]
          [name name]
          [kind kind]
          [ty ty]
        ]]]
      ]

      // * Register into namespace
      [cxt . Cxt.export_name name uid]
      [cxt . Cxt.open_name name uid]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" PExpr.any_sym]]
    . <*> [PList.one_of [PExpr.label "<kind>" PExpr.any_expr]]
    . <*> [PList.one_of [PExpr.label "<type>" PExpr.any_expr]]
  ]]]
]