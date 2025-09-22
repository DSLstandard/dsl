[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/elab_tm.dsl"]
  [import "dsl/elab_ty.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/pexpr.dsl"]
  [import "dsl/types.dsl"]
  [import "wlp.dsl"]

  [def-decl ref_def_or_report [-> Cxt Expr Name [Maybe XDef]]]
  [def-impl ref_def_or_report [\ cxt name_expr name [do
    [else-let [Some uid] [cxt . Cxt.lookup_namespace name] [do
      [let msg [[Doc.of [type Void] "Unknown def declaration: "] . <> [Doc.squotes [Doc.of name]]]]
      [cxt . Cxt.mark_expr_error name_expr msg]
      [None]
    ]]

    [else-let [Def def] [cxt . Cxt.db . Database.get_entry uid] [do
      [cxt . Cxt.mark_expr_error name_expr [Doc.of "Should refer to a def declaration"]]
      [None]
    ]]

    [Some def]
  ]]]

  [def-decl pub pat_def_decl [PExpr [-> Cxt ,]]]
  [def-impl pat_def_decl [PExpr.block_ "def-decl" [
    [\ is_pub is_auto name ty_expr cxt [do
      [let ty [check_ty cxt ty_expr Kind.Star]]

      [let uid [cxt . Cxt.db . Database.fresh_uid]]
      [cxt . Cxt.db . Database.insert_entry
        [XEntry.Def [XDef.new* [dict
          [uid uid]
          [name name]
          [is_auto is_auto]
          [annotations [Array.create ,]]
          [ty ty]
          [tm [var [None]]]
        ]]]
      ]

      [cxt . Cxt.open_name name uid]
      [when is_pub [\ _ [do
        [cxt . Cxt.export_name name uid]
      ]]]
    ]]
    . <$> [PList.modifier [PExpr.sym "pub"]]
    . <*> [PList.modifier [PExpr.sym "auto"]]
    . <*> [PList.item [PExpr.label "<name>" PExpr.any_sym]]
    . <*> [PList.item [PExpr.label "<type>" PExpr.any_expr]]
  ]]]

  [def-decl pub pat_def_annotate [PExpr [-> Cxt ,]]]
  [def-impl pat_def_annotate [PExpr.block_ "def-annotate" [
    [\ [input [; Expr Sym]] [annotation String] [cxt Cxt] [do
      [let= [; name_expr name] input]
      [let= [Some def] [ref_def_or_report cxt name_expr name]
        [[None] ,]]
      [def . XDef.annotations . Array.append annotation]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" [PExpr.capture PExpr.any_sym]]]
    . <*> [PList.one_of [PExpr.label "<annotation>" PExpr.any_str]]
  ]]]

  [def-decl pub pat_def_impl [PExpr [-> Cxt ,]]]
  [def-impl pat_def_impl [PExpr.block_ "def-impl" [
    [\ [input [; Expr Sym]] [tm_expr Expr] [cxt Cxt] [do
      [let= [; name_expr name] input]
      [let= [Some def] [ref_def_or_report cxt name_expr name]
        [[None] ,]]
      [let ty [eval_ty [Cxt.eval cxt] [Env.nil] [def . XDef.ty]]]
      [let tm [check_tm cxt tm_expr ty]]
      [def . XDef.tm . set [Some tm]]
    ]]
    . <$> [PList.one_of [PExpr.label "<name>" [PExpr.capture PExpr.any_sym]]]
    . <*> [PList.one_of [PExpr.label "<body>" PExpr.any_expr]]
  ]]]
]