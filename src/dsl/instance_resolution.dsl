[file
  [import "astar_monad.dsl"]
  [import "base.dsl"]
  [import "colog.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/cxt_utils.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/ty_has_meta.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "dsl/unify_ty.dsl"]
  [import "free.dsl"]
  [import "wlp.dsl"]

  [class-decl Searcher]
  [class-struct Searcher
    // Pins the location for error-reporting
    [field cxt Cxt]
    // The type to make an instance of
    [field expr_ref Expr]
  ]

  [def-decl delay_one_step [AStar I32 ,]]
  [def-impl delay_one_step [do
    [AStar.update_g_cost I32.suc]
  ]]

  [def-decl abandon_unless_unifies [-> Searcher Vy Vy [AStar I32 ,]]]
  [def-impl abandon_unless_unifies [\ sr ty1 ty2 [do
    [try _ [AStar.lazily]]

    [let cxt [sr . Searcher.cxt]]
    [match [unify_ty cxt ty1 ty2]
      [[Fail] [do
        // Usually if unification fails, we abandon immediately. This is because
        // it is usually a sign that the user did not provide enough type hints
        // for the compiler.
        [AStar.abandon]
      ]]
      [[Ok] [do
        [pure ,]
      ]]
    ]
  ]]]

  // Usually if we encounter an unsolved meta, we abandon immediately. This is
  // because it is usually a sign that the user did not provide enough type
  // hints for the compiler.
  //
  // The caller has the responsibility of forcing the input type.
  [def-decl abandon_if_is_flex [-> Vy [AStar I32 ,]]]
  [def-impl abandon_if_is_flex [\ ty [do
    [try _ [AStar.lazily]]

    [match ty
      [[Flex] [do
        [AStar.abandon]
      ]]
      [_ [do
        [pure ,]
      ]]
    ]
  ]]]

  // If the input type is considered "ugly" (i.e. it has unsolved metas), we
  // abandon immediately. This is generally used to prevent the instance
  // resolution from attempting any further with us knowing that it will not work
  // properly when holes are present.
  [def-decl abandon_if_type_is_ugly [-> Searcher Vy [AStar I32 ,]]]
  [def-impl abandon_if_type_is_ugly [\ sr ty [do
    [try _ [AStar.lazily]]

    [let cxt [sr . Searcher.cxt]]
    [if [check_ty_has_meta cxt ty]
      [AStar.abandon]
      [pure ,]
    ]
  ]]]

  // Decomposes the root tuple type into its element types. Forks the search for
  // each element type.
  [def-decl decompose_tuple [-> Searcher Tm Vy [AStar I32 [; Tm Vy]]]]
  [def-impl decompose_tuple [\ sr in_tm in_ty [do
    [try _ [AStar.lazily]]

    [let cxt [sr . Searcher.cxt]]
    [let in_ty [force_ty_weak [Cxt.eval cxt] in_ty]]
    [match in_ty
      [[Tuple elem_tys] [do
        [try entry [AStar.pick [elem_tys . Array.iter . Iter.ixed]]]

        [let= [; i elem_ty] entry]
        [let elem_tm [Tm.GetTupleIndex in_tm i]]

        // We also want to decompose `(A, (B, C)))` into `A`, `B`, and `C`
        // instead of just `A` and `(B, C)`.
        [decompose_tuple sr elem_tm elem_ty]
      ]]
      [_ [do
        [pure [; in_tm in_ty]]
      ]]
    ]
  ]]]

  [def-decl solve_tm [-> Searcher Vy [AStar I32 Tm]]]
  [def-decl solve_tm_app [-> Searcher Vy [AStar I32 Tm] Vy [AStar I32 Tm]]]

  [def-impl solve_tm [\ sr want_ty [do
    [try _ [abandon_if_type_is_ugly sr want_ty]]
    [try _ delay_one_step]

    [let cxt [sr . Searcher.cxt]]

    [if-let [Tuple elem_tys] want_ty [do
      // Special case: For tuples, we automatically search for instances of each
      // element (like Haskell).
      [let elems [Array.create ,]]
      [try _ [elem_tys . Array.iter . Iter.traverse_ [\ elem_ty [do
        [try elem_tm [solve_tm sr elem_ty]]
        [elems . Array.append elem_tm]
        [pure continue]
      ]]]]
      [pure [Tm.Tuple elems]]
    ]]

    [let global_branches [AStar.lazy [\ _ [do
      [let uids [cxt . Cxt.db . Database.iter_auto_def_uids . Array.from_iter]]

      [try def_uid [AStar.pick [uids . Array.iter]]]
      [let def [cxt . Cxt.db . Database.get_def def_uid]]
      [let def_ty [eval_ty [Cxt.eval cxt] [Env.nil] [def . XDef.ty]]]
      [let def_tm [Tm.Def def_uid]]

      [solve_tm_app sr def_ty [pure def_tm] want_ty]
    ]]]]

    [let local_branches [AStar.lazy [\ _ [do
      [try bound_l [AStar.pick [cxt . Cxt.tm_bound_autos . List.iter]]]
      [let bound_i [bound_l . lvl_to_ix [cxt . Cxt.tm_scope . Scope.lvl]]]
      [let bound_tm [Tm.var bound_i]]
      [let bound_ty [cxt . Cxt.tm_scope . Scope.at bound_i]]

      [try cand [decompose_tuple sr bound_tm bound_ty]]
      [let= [; cand_tm cand_ty] cand]

      [try _ [abandon_if_type_is_ugly sr cand_ty]]
      [try _ [abandon_unless_unifies sr want_ty cand_ty]]

      [pure cand_tm]
    ]]]]

    [AStar.fork [Iter.two local_branches global_branches]]
  ]]]

  [def-impl solve_tm_app [\ sr app_ty mk_app_tm exp_ret_ty [do
    [let cxt [sr . Searcher.cxt]]
    [let app_ty [force_ty_weak [Cxt.eval cxt] app_ty]]
    [match app_ty
      [[Flex] [do
        // Usually if we encounter an unsolved meta, we abandon immediately.
        // This is because it is usually a sign that the user did not provide
        // enough type hints for the compiler.
        [AStar.abandon]
      ]]
      [[Forall [; kind cod]] [do
        // Plug foralls
        [let t [Cxt.fresh_ty_meta cxt kind [MetaReason.new [sr . Searcher.expr_ref] [Doc.of "type hole for instance resolution"]]]]
        [let cod [eval_ty_closure [Cxt.eval cxt] cod t]]
        [solve_tm_app sr cod mk_app_tm exp_ret_ty]
      ]]
      [[Arr [; [Auto] dom cod]] [do
        // Plug auto args
        [let mk_res_tm [AStar.lazy [\ _ [do
          [try f mk_app_tm]
          [try x [solve_tm sr dom]]
          [pure [Tm.app f x]]
        ]]]]
        [solve_tm_app sr cod mk_res_tm exp_ret_ty]
      ]]
      [app_ty [do
        // Unify immediately to gain type information so the auto 'solve_tm's in
        // 'mk_app_tm' can better properly.
        [try _ [abandon_unless_unifies sr app_ty exp_ret_ty]]
        mk_app_tm
      ]]
    ]
  ]]]

  // Returns 'Nothing' if no instance could be found
  [def-decl pub resolve_instance [-> Cxt Expr Vy [Maybe Tm]]]
  [def-impl resolve_instance [\ cxt expr_ref want_ty [do
    [let sr [Searcher.new* [dict
      [cxt cxt]
      [expr_ref expr_ref]
    ]]]

    [when [cxt . Cxt.debugging] [\ _ [do
      [let want_ty_doc [Cxt.render_vy cxt want_ty]]
      [cxt . Cxt.debug_out . LogAction.run
        [Doc.of "[resolve_instance] <<< Attempting to solve for type: " . <> want_ty_doc]]
    ]]]

    [let sol [AStar.find_best_ [AStarParams.by_i32_cost
      [AStar.lazy [\ _ [solve_tm sr want_ty]]]
    ]]]

    [when [cxt . Cxt.debugging] [\ _ [do
      [let want_ty_doc [Cxt.render_vy cxt want_ty]]
      [let success [sol . Maybe.is_some]]
      [cxt . Cxt.debug_out . LogAction.run [Foldable.mconcat [vec
        [Doc.of "[resolve_instance] >>> Finished search for type: "]
        want_ty_doc
        [Doc.of ". Success = "]
        [Doc.of success]
      ]]]
    ]]]

    sol
  ]]]
]