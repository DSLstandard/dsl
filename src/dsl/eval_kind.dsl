[file
  [import "base.dsl"]
  [import "dsl/eval_cxt.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/types.dsl"]

  // * Forcing

  [def-decl try_force_kind_meta [-> EvalCxt KindMetaID [Maybe Kind]]]
  [def-impl try_force_kind_meta [\ cxt m
    [cxt . EvalCxt.metas . MetaStore.get_kind_meta m . KindMeta.solution]
  ]]

  [def-decl pub force_kind_weak [-> EvalCxt Kind Kind]]
  [def-impl force_kind_weak [\ cxt kind [do
    [let= [Meta m] kind
      [_ kind]]
    [let= [Some kind] [try_force_kind_meta cxt m]
      [_ kind]]
    [force_kind_weak cxt kind]
  ]]]

  [def-decl pub force_kind [-> EvalCxt Kind Kind]]
  [def-impl force_kind [\ cxt kind [do
    [match kind
      [[Star] Kind.Star]
      [[Arr [; dom cod]] [do
        [let dom [force_kind cxt dom]]
        [let cod [force_kind cxt cod]]
        [Kind.Arr dom cod]
      ]]
      [[Meta m] [do
        [let= [Meta m] kind
          [_ kind]]
        [let= [Some kind] [try_force_kind_meta cxt m]
          [_ kind]]
        [force_kind cxt kind]
      ]]
    ]
  ]]]
]
