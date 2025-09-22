[file
  [import "base.dsl"]
  [import "colog.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/cxt_utils.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/eval_cxt.dsl"]
  [import "dsl/eval_kind.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/pretty_ty.dsl"]
  [import "dsl/quote_ty.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/pretty_kind.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "dsl/unify_kind.dsl"]
  [import "dsl/unify_ty_miller.dsl"]
  [import "wlp.dsl"]

  [class-decl UnifyTyResult]
  [class-enum UnifyTyResult
    [member Ok ,]
    [member Fail ,]
  ]

  [def-decl pub unify_ty [-> Cxt Vy Vy UnifyTyResult]]

  // NOTE: Just 'TyLvl' is not enough. We need the whole '[Env Kind]' because
  // rigid imitation has to inspect kind-level info.
  [def-decl unify_ty.go [-> Cxt [Env Kind] Vy Vy UnifyTyResult]]
  [def-decl unify_ty.go.spine [-> Cxt [Env Kind] [Spine Vy] [Spine Vy] UnifyTyResult]]
  [def-decl unify_ty.go.miller [-> Cxt [Env Kind] [; TyMetaID [Spine Vy]] Vy UnifyTyResult]]
  [def-decl unify_ty.go.try_imitate_rigid [-> Cxt [Env Kind] Vy Vy [Maybe UnifyTyResult]]]

  [def-impl unify_ty [\ cxt lhs rhs [do
    [when [cxt . Cxt.debugging] [\ _ [do
      [let lhs_doc [Cxt.render_vy cxt lhs]]
      [let rhs_doc [Cxt.render_vy cxt rhs]]

      [cxt . Cxt.debug_out . LogAction.run [Foldable.mconcat [vec
        [Doc.of "[unify_ty] "]
        lhs_doc
        [Doc.of " ~ "]
        rhs_doc
      ]]]
    ]]]

    [unify_ty.go cxt [cxt . Cxt.ty_scope . Scope.elem_env] lhs rhs]
  ]]]

  [def-impl unify_ty.go [\ cxt env lhs rhs [do
    [let lhs [force_ty_weak [Cxt.eval cxt] lhs]]
    [let rhs [force_ty_weak [Cxt.eval cxt] rhs]]

    [let handle_eta_expand [\ [kind Kind] [lhs_c TyClosure] [rhs Vy] [do
      [let arg [Vy.bound [env . Env.lvl]]]

      [let lhs* [eval_ty_closure [Cxt.eval cxt] lhs_c arg]]
      [let rhs* [vy_app [Cxt.eval cxt] rhs arg]]

      [unify_ty.go cxt [Env.push env kind] lhs* rhs*]
    ]]]
    
    // Handle trivial cases with pattern matching if possible
    [else-let= _ [; lhs rhs]
      // Trivial rigid-rigid
      [[; [Rigid [; h1 sp1]] [Rigid [; h2 sp2]]] [do
        [if-let [false] [h1 . == h2]
          UnifyTyResult.Fail]

        [unify_ty.go.spine cxt env sp1 sp2]
      ]]

      // Trivial tuple-tuple
      [[; [Tuple elems1] [Tuple elems2]] [do
        [if-let [false] [== [Array.length elems1] [Array.length elems2]]
          UnifyTyResult.Fail]

        [let pairs [Iter.zip [elems1 . Array.iter] [elems2 . Array.iter]]]

        [let* [go [\ _ [do
          [match [pairs . Iter.next?]
            [[None] UnifyTyResult.Ok]
            [[Some [; a b]] [do
              [if-let [Fail] [unify_ty.go cxt env a b] [do
                [UnifyTyResult.Fail]
              ]]
              [go ,]
            ]]
          ]
        ]]]]
        [go ,]
      ]]

      // Trivial dict-dict
      [[; [Dict d1] [Dict d2]] [do
        // Dict keys must align
        [let keys [d1 . StringMap.keys_set]]
        [if-let [false] [keys . == [d2 . StringMap.keys_set]] UnifyTyResult.Fail]

        // Paired dict values must unify
        [let keys [keys . StringSet.iter]]
        [let* [go [\ _ [do
          [match [keys . Iter.next?]
            [[None] UnifyTyResult.Ok]
            [[Some key] [do
              [let v1 [d1 . StringMap.get key]]
              [let v2 [d2 . StringMap.get key]]

              [if-let [Fail] [unify_ty.go cxt env v1 v2] [do
                UnifyTyResult.Fail
              ]]

              [go ,]
            ]]
          ]
        ]]]]
        [go ,]
      ]]

      // Trivial arr-arr
      [[; [Arr [; icit1 dom1 cod1]] [Arr [; icit2 dom2 cod2]]] [do
        [if-let [false] [== icit1 icit2] UnifyTyResult.Fail]
        [if-let [Fail] [unify_ty.go cxt env dom1 dom2] UnifyTyResult.Fail]
        [unify_ty.go cxt env cod1 cod2]
      ]]

      // Trivial lam-lam
      [[; [Lam [; kind1 c1]] [Lam [; kind2 c2]]] [do
        [if-let [Fail] [unify_kind cxt kind1 kind2] UnifyTyResult.Fail]
        [let arg [Vy.bound [Env.lvl env]]]
        [let c1 [eval_ty_closure [Cxt.eval cxt] c1 arg]]
        [let c2 [eval_ty_closure [Cxt.eval cxt] c2 arg]]
        [unify_ty.go cxt [env . Env.push kind1] c1 c2]
      ]]

      // Eta expansion
      [[; [Lam [; kind lhs_c]] rhs] [handle_eta_expand kind lhs_c rhs]]
      [[; lhs [Lam [; kind rhs_c]]] [handle_eta_expand kind rhs_c lhs]]

      // Trivial forall-forall
      [[; [Forall [; kind1 c1]] [Forall [; kind2 c2]]] [do
        [if-let [Fail] [unify_kind cxt kind1 kind2] UnifyTyResult.Fail]
        [let arg [Vy.bound [Env.lvl env]]]
        [let c1 [eval_ty_closure [Cxt.eval cxt] c1 arg]]
        [let c2 [eval_ty_closure [Cxt.eval cxt] c2 arg]]
        [unify_ty.go cxt [env . Env.push kind1] c1 c2]
      ]]

      // Flex-flex with the same meta head
      [[; [Flex [; m1 sp1]] [Flex [; m2 sp2]]] [where [== m1 m2]] [do
        // FIXME: Placeholder implementation. See elaboration-zoo on pruning
        [unify_ty.go.spine cxt env sp1 sp2]
      ]]

      // Pattern unification
      [[; [Flex [as m_side [; _ m_sp]]] other_side] [where [miller.can_invert_spine cxt m_sp]]
        [unify_ty.go.miller cxt env m_side other_side]]
      [[; other_side [Flex [as m_side [; _ m_sp]]]] [where [miller.can_invert_spine cxt m_sp]]
        [unify_ty.go.miller cxt env m_side other_side]]
    ]

    // Try "Rigid imitation" heuristic (TODO: Find academic references on this)
    [if-let [Some result] [unify_ty.go.try_imitate_rigid cxt env lhs rhs] result]
    [if-let [Some result] [unify_ty.go.try_imitate_rigid cxt env rhs lhs] result]

    // Fail if nothing works
    UnifyTyResult.Fail
  ]]]

  [def-impl unify_ty.go.spine [\ cxt env sp1 sp2 [do
    [if-let [false] [== [Spine.length sp1] [Spine.length sp2]] UnifyTyResult.Fail]

    [let* [go [\ [i I32] [do
      [if-let [true] [i . == [Spine.length sp1]]
        UnifyTyResult.Ok]

      [let a [sp1 . Spine.at i]]
      [let b [sp2 . Spine.at i]]
      [if-let [Fail] [unify_ty.go cxt env a b] UnifyTyResult.Fail]

      [go [i . I32.suc]]
    ]]]]
    [go 0]
  ]]]

  [def-impl unify_ty.go.miller [\ cxt env m_side other_side [do
    [let= [; m_id m_sp] m_side]

    [let pren [miller.invert cxt env m_sp
      . Maybe.expect "unify_ty.go.miller: invert should never fail because of guard check"]]

    [else-let [Some sol] [miller.rename cxt m_id pren other_side] [do
      UnifyTyResult.Fail
    ]]

    [let sol [Ty.fold_lams_by_env [pren . PartialRenaming.dom_env] sol]]
    [let sol [eval_ty [Cxt.eval cxt] [Env.nil] sol]]
    [cxt . Cxt.set_ty_meta_solution m_id sol]

    UnifyTyResult.Ok
  ]]]

  [def-impl unify_ty.go.try_imitate_rigid [\ cxt env m_side rigid_side [do
    // Problems to consider:
    //   Case 1:
    //     __then__ : (type M : * -> *) -> (type A : *) -> (type B -> *) -> M A -> (A -> M B) -> M B
    //     __then__(?1, ?2, ?3, Id 123, \(x) => Id x)
    //     ... We should unify: ?1 ?3 = Id 123
    //
    //   Case 2:
    //     s <- getState()
    //     - has type:     State ?322 ?322
    //     - should match: ?319       ?320
    //
    //     ... We should unify: ?319 = State ?322
    //
    //   Case 3:
    //     has type:     ?882 V A B (?884 V A B)
    //     should match: Yield V    B
    //
    //     ... We should unify: ?882 V A B = Yield V
    //
    //   Case 4:
    //     `forListM_(i + 1, list, body)`
    //
    //     has type: ?910 T M ()
    //     should match: M ()
    //
    //     ... We should unify: ?910 T M = M
    //
    //   In general:
    //     has type:     ?m i1 i2 i3 i4 ...
    //     should match: (C x1 x2 x3 x4 ...) : *

    [else-let [Flex [; m_id m_sp]] m_side [None]]
    [else-let [Rigid [; r_head r_sp]] rigid_side [None]]

    [let r_head_kind [match r_head
      [[Bound l] [do
        [let i [l . lvl_to_ix [Env.lvl env]]]
        [env . Env.at i]
      ]]
      [[Known known] [do
        [known . KnownType.kind]
      ]]
      [[Class uid] [do
        [let class [cxt . Cxt.db . Database.get_class uid]]
        [class . XClass.compute_kind]
      ]]
    ]]

    [let r_head_kind [force_kind [Cxt.eval cxt] r_head_kind]]
    [if-let [false] [Kind.is_arity_known r_head_kind] [None]]

    [let meta [cxt . Cxt.metas . MetaStore.get_ty_meta m_id]]
    [let m_head_kind [force_kind [Cxt.eval cxt] [meta . TyMeta.kind]]]
    [if-let [false] [Kind.is_arity_known m_head_kind] [None]]

    // FIXME: Optimize this
    [try m_sp0 [m_sp
      . Spine.seq
      . Seq.inits
      . Iter.map [Spine.of_seq]
      . Array.from_iter
      . Array.iter_reversed
      . Iter.find_some [\ m_sp0 [do
        [if [miller.can_invert_spine cxt m_sp0]
          [Some m_sp0]
          [None]]
      ]]
    ]]

    [let m_sp_len [Spine.length m_sp]]
    [let m_sp0_len [Spine.length m_sp0]]
    [let m_sp1_len [I32.-  m_sp_len m_sp0_len]]

    [let r_sp_len [Spine.length r_sp]]
    [let r_sp0_len [I32.- r_sp_len m_sp1_len]]

    [try _ [Maybe.guard [r_sp0_len . >= 0]]]
    
    [let r_sp0 [r_sp . Spine.take r_sp0_len]]

    [let uk_res [unify_kind cxt
      [r_head_kind . Kind.drop_arr r_sp0_len]
      [m_head_kind . Kind.drop_arr m_sp0_len]
    ]]
    [if-let [Fail] uk_res
      [Some UnifyTyResult.Fail]]

    // Imitate then retry the original unification should yield new results.
    [if-let [Fail] [unify_ty.go cxt env [Vy.Rigid r_head r_sp0] [Vy.Flex m_id m_sp0]]
      [Some UnifyTyResult.Fail]]
    [if-let [Fail] [unify_ty.go cxt env [Vy.Rigid r_head r_sp] [Vy.Flex m_id m_sp]]
      [Some UnifyTyResult.Fail]]

    [Some UnifyTyResult.Ok]
  ]]]
]