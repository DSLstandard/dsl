[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/cxt_utils.dsl"]
  [import "dsl/eval_cxt.dsl"]
  [import "dsl/eval_kind.dsl"]
  [import "dsl/kind_utils.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/types.dsl"]

  [class-decl UnifyKindResult]
  [class-enum UnifyKindResult
    [member Ok ,]
    [member Fail ,]
  ]

  [def-decl pub unify_kind [-> Cxt Kind Kind UnifyKindResult]]
  [def-impl unify_kind [\ cxt k1 k2 [do
    [let solve [\ [id KindMetaID] [kind Kind] [do
      [let kind [force_kind [Cxt.eval cxt] kind]] // Force to reveal all the unsolved metas.

      // Occurrence check
      [if-let [true] [Kind.scan_meta_ids kind . I32MutSet.has id] [do
        UnifyKindResult.Fail
      ]]

      [cxt . Cxt.set_kind_meta_solution id kind]
      UnifyKindResult.Ok
    ]]]

    [let k1 [force_kind [Cxt.eval cxt] k1]]
    [let k2 [force_kind [Cxt.eval cxt] k2]]
    [match [; k1 k2]
      [[; [Star] [Star]] [do
        [UnifyKindResult.Ok]
      ]]
      [[; [Arr [; dom1 cod1]] [Arr [; dom2 cod2]]] [do
        [let= [Ok] [unify_kind cxt dom1 dom2]
          [_ UnifyKindResult.Fail]]
        [let= [Ok] [unify_kind cxt cod1 cod2]
          [_ UnifyKindResult.Fail]]
        UnifyKindResult.Ok
      ]]
      [[; [Meta m] rhs] [solve m rhs]]
      [[; lhs [Meta m]] [solve m lhs]]
      [_ UnifyKindResult.Fail]
    ]
  ]]]
]