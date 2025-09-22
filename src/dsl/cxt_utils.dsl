[file
  [import "base.dsl"]
  [import "colog.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/eval_kind.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/location.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/pretty_kind.dsl"]
  [import "dsl/pretty_ty.dsl"]
  [import "dsl/quote_ty.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "wlp.dsl"]

  [def-decl pub Cxt.render_vy [-> Cxt Vy [Doc Void]]]
  [def-impl Cxt.render_vy [\ cxt ty [do
    [let ty [force_ty [Cxt.eval cxt] ty]]
    [let ty [quote_ty [Cxt.eval cxt] [cxt . Cxt.ty_scope . Scope.lvl] ty]]
    [pretty_ty [Cxt.db cxt] [cxt . Cxt.ty_scope . Scope.name_env] ty]
  ]]]

  // Like 'Cxt.render_vy' but for 'Vy' with no free bound variables.
  [def-decl pub Cxt.render_constant_vy [-> Cxt Vy [Doc Void]]]
  [def-impl Cxt.render_constant_vy [\ cxt ty [do
    [let ty [force_ty [Cxt.eval cxt] ty]]
    [let ty [quote_ty [Cxt.eval cxt] 0 ty]]
    [pretty_ty [Cxt.db cxt] [Env.nil] ty]
  ]]]

  [def-decl pub Cxt.render_kind [-> Cxt Kind [Doc Void]]]
  [def-impl Cxt.render_kind [\ cxt kind [do
    [let kind [force_kind [Cxt.eval cxt] kind]]
    [pretty_kind kind]
  ]]]

  // Generate a fresh type meta nad writes debug info.
  [def-decl pub Cxt.fresh_ty_meta [-> Cxt Kind MetaReason Vy]]
  [def-impl Cxt.fresh_ty_meta [\ cxt kind reason [do
    [let id [cxt . Cxt.metas . MetaStore.fresh_meta_id ,]]
    [let kind* [cxt . Cxt.ty_scope . TyScope.fold_as_kind_arr kind]]
    [let meta [TyMeta.new* [dict
      [id id]
      [kind kind*]
      [reason reason]
      [solution [None]]
    ]]]
    [cxt . Cxt.metas . MetaStore.create_ty_meta meta]

    [when [cxt . Cxt.debugging] [\ _ [do
      [cxt . Cxt.debug_out . LogAction.run [Foldable.mconcat [vec
        [Doc.of "[Cxt.fresh_ty_meta] ?"]
        [Doc.of id]
        [Doc.of " : "]
        [Cxt.render_kind cxt kind]
        [Doc.of " "]
        [Doc.of [reason . MetaReason.expr . Expr.loc] . Doc.parens]
        [Doc.of " ... "]
        [reason . MetaReason.desc]
      ]]]
    ]]]

    [Vy.Flex id [Spine.of_ty_bounds [cxt . Cxt.ty_scope . Scope.lvl]]]
  ]]]

  [def-decl pub Cxt.fresh_kind_meta [-> Cxt MetaReason Kind]]
  [def-impl Cxt.fresh_kind_meta [\ cxt reason [do
    [let id [cxt . Cxt.metas . MetaStore.fresh_meta_id ,]]
    [cxt . Cxt.metas . MetaStore.create_kind_meta [KindMeta.new* [dict
      [id id]
      [solution [None]]
      [reason reason]
    ]]]

    [when [cxt . Cxt.debugging] [\ _ [do
      [cxt . Cxt.debug_out . LogAction.run [Foldable.mconcat [vec
        [Doc.of "[Cxt.fresh_kind_meta] ?"]
        [Doc.of id]
        [Doc.of " "]
        [Doc.of [reason . MetaReason.expr . Expr.loc] . Doc.parens]
        [Doc.of " ... "]
        [reason . MetaReason.desc]
      ]]]
    ]]]

    [Kind.Meta id]
  ]]]

  // Like 'MetaStore.set_ty_meta_solution' but also writes debug info.
  [def-decl pub Cxt.set_ty_meta_solution [-> Cxt TyMetaID Vy ,]]
  [def-impl Cxt.set_ty_meta_solution [\ cxt m_id solution [do
    [when [cxt . Cxt.debugging] [\ _ [do
      [cxt . Cxt.debug_out . LogAction.run [Foldable.mconcat [vec
        [Doc.of "[Cxt.set_ty_meta_solution] ?"]
        [Doc.of m_id]
        [Doc.of " := "]
        [Cxt.render_constant_vy cxt solution]
      ]]]
    ]]]

    [Cxt.metas cxt . MetaStore.set_ty_meta_solution m_id solution]
  ]]]

  // Like 'MetaStore.set_kind_meta_solution' but also writes debug info.
  [def-decl pub Cxt.set_kind_meta_solution [-> Cxt KindMetaID Kind ,]]
  [def-impl Cxt.set_kind_meta_solution [\ cxt m_id solution [do
    [when [cxt . Cxt.debugging] [\ _ [do
      [cxt . Cxt.debug_out . LogAction.run [Foldable.mconcat [vec
        [Doc.of "[Cxt.set_kind_meta_solution] ?"]
        [Doc.of m_id]
        [Doc.of " := "]
        [Cxt.render_kind cxt solution]
      ]]]
    ]]]

    [Cxt.metas cxt . MetaStore.set_kind_meta_solution m_id solution]
  ]]]
]
