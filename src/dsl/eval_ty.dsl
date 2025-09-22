[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/eval_cxt.dsl"]
  [import "dsl/eval_kind.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]

  [def-decl pub eval_ty [-> EvalCxt [Env Vy] Ty Vy]]
  [def-decl pub eval_ty_closure [-> EvalCxt TyClosure Vy Vy]]
  [def-decl pub vy_app [-> EvalCxt Vy Vy Vy]]
  [def-decl pub vy_app_spine [-> EvalCxt Vy [Spine Vy] Vy]]

  [def-impl eval_ty [\ cxt env ty [do
    [match ty
      [[Meta m] [Vy.meta m]]

      [[Var i] [do
        // A very common error during compiler development, worth a
        // dedicated check.
        [when [i . < 0] [\ _ [do
          [panic ["eval_ty: bad variable index: "
            . <> [I32.to_str i]
            . <> " (env lvl = " . <> [I32.to_str [Env.lvl env]]
            . <> ")"]]
        ]]]

        [env . Env.at i]
      ]]
      [[Known i] [Vy.known i]]
      [[Class u] [Vy.class u]]

      [[App [; f x]] [do
        [let f [eval_ty cxt env f]]
        [let x [eval_ty cxt env x]]
        [vy_app cxt f x]
      ]]
      [[Def uid] [eval_ty cxt env [cxt . EvalCxt.db . Database.get_tydef uid . XTyDef.ty]]]

      [[Tuple elems] [Vy.Tuple [elems . Array.map [eval_ty cxt env]]]]
      [[Dict ds] [Vy.Dict [ds . StringMap.map [eval_ty cxt env]]]]

      [[Arr [; icit dom cod]] [do
        [let dom [eval_ty cxt env dom]]
        [let cod [eval_ty cxt env cod]]
        [Vy.Arr icit dom cod]
      ]]

      [[Forall [; kind e]] [Vy.Forall kind [TyClosure.new env e]]]
      [[Lam [; kind e]] [Vy.Lam kind [TyClosure.new env e]]]
    ]
  ]]]

  [def-impl eval_ty_closure [\ cxt closure arg [do
    [eval_ty cxt
      [closure . TyClosure.env . Env.push arg]
      [closure . TyClosure.ty]
    ]
  ]]]

  [def-impl vy_app [\ cxt f x [do
    [vy_app_spine cxt f [Spine.one x]]
  ]]]

  [def-impl vy_app_spine [\ cxt f in_sp [do
    [match f
      [[Flex [; m sp]] [Vy.Flex m [sp . Spine.<> in_sp]]]
      [[Rigid [; h sp]] [Vy.Rigid h [sp . Spine.<> in_sp]]]
      [[Lam [; kind c]]
        [match [in_sp . Spine.uncons]
          [[None] f]
          [[Some [; x xs]] [vy_app_spine cxt [eval_ty_closure cxt c x] xs]]
        ]
      ]
      [_
        [if [in_sp . Spine.is_empty]
          f
          [panic "vy_app_spine: not a function (spine is not empty)"]
        ]
      ]
    ]
  ]]]

  // * Forcing

  [def-decl try_force_ty_meta [-> EvalCxt TyMetaID [Spine Vy] [Maybe Vy]]]
  [def-impl try_force_ty_meta [\ cxt m_id m_sp [do
    [try m_sol [cxt . EvalCxt.metas . MetaStore.get_ty_meta m_id . TyMeta.solution]]
    [pure [vy_app_spine cxt m_sol m_sp]]
  ]]]

  [def-decl pub force_ty_weak [-> EvalCxt Vy Vy]]
  [def-impl force_ty_weak [\ cxt in_ty [do
    [else-let [Flex [; m_id m_sp]] in_ty
      in_ty]
    [else-let [Some forced_ty] [try_force_ty_meta cxt m_id m_sp]
      in_ty]
    [force_ty_weak cxt forced_ty]
  ]]]

  [def-decl pub force_ty [-> EvalCxt Vy Vy]]
  [def-impl force_ty [\ cxt in_ty
    [match in_ty
      [[Flex [; m_id m_sp]] [do
        [match [try_force_ty_meta cxt m_id m_sp]
          [[Some ty] [force_ty cxt ty]]
          [[None] [Vy.Flex m_id [Spine.map m_sp [force_ty cxt]]]]
        ]
      ]]
      [[Rigid [; head sp]] [do
        [let sp [sp . Spine.map [force_ty cxt]]]
        [Vy.Rigid head sp]
      ]]

      [[Tuple elems] [Vy.Tuple [elems . Array.map [force_ty cxt]]]]
      [[Dict ds] [Vy.Dict [ds . StringMap.map [force_ty cxt]]]]

      [[Arr [; icit dom cod]] [do
        [let dom [force_ty cxt dom]]
        [let cod [force_ty cxt cod]]
        [Vy.Arr icit dom cod]
      ]]
      [[Forall [; kind c]] [do
        [Vy.Forall [force_kind cxt kind] c]
      ]]
      [[Lam [; kind c]] [do
        [Vy.Lam [force_kind cxt kind] c]
      ]]
    ]
  ]]
]