[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]

  // Returns true if the input type contains any unsolved metas.
  [def-decl pub check_ty_has_meta [-> Cxt Vy Bool]]
  [def-impl check_ty_has_meta [\ cxt in_ty [do
    [let*
      [has_meta? [\ [lvl TyLvl] [ty Vy] [:: [type Bool] [do
        [let ty [force_ty_weak [Cxt.eval cxt] ty]]
        [match ty
          [[Flex] true]
          [[Rigid [; _ sp]] [do
            [sp . Spine.iter . Iter.any [has_meta? lvl]]
          ]]
          [[Tuple elems] [do
            [elems . Array.iter . Iter.any [has_meta? lvl]]
          ]]
          [[Dict d] [do
            [d . StringMap.iter_values . Iter.any [has_meta? lvl]]
          ]]
          [[Arr [; _ dom cod]] [do
            [Iter.two dom cod . Iter.any [has_meta? lvl]]
          ]]
          [[Forall [; kind cod]] [do
            [let cod [eval_ty_closure [Cxt.eval cxt] cod [Vy.bound lvl]]]
            [has_meta? [I32.suc lvl] cod]
          ]]
          [[Lam [; kind cod]] [do
            [let cod [eval_ty_closure [Cxt.eval cxt] cod [Vy.bound lvl]]]
            [has_meta? [I32.suc lvl] cod]
          ]]
        ]
      ]]]]
    ]

    [has_meta? [cxt . Cxt.ty_scope . Scope.lvl] in_ty]
  ]]]
]