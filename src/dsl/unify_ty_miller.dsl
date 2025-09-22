[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]

  // If we have `env |- (?m sp = rhs)`, then:
  // - env is referred to as the codomain's environment
  // - cod_lvl = 'env . Env.lvl'
  // - dom_env is constructed by what 'sp' bounds are. Its lvl should equal 'sp.
  //   Spine.length'
  // - renaming is a partial function from codomain lvl to domain lvl, but as a
  //   hashmap.
  [class-decl PartialRenaming]
  [class-struct PartialRenaming
    [field dom_env [Env Kind]]
    [field cod_lvl TyLvl]

    // Codomain lvl -> Domain lvl
    [field renaming [I32Map TyLvl]]
  ]

  [def-decl pub PartialRenaming.lift [-> Kind PartialRenaming PartialRenaming]]
  [def-impl PartialRenaming.lift [\ kind in_pren [do
    [let= [mk [; dom_env cod_lvl renaming]] in_pren]
    [PartialRenaming.new* [dict
      [dom_env [dom_env . Env.push kind]]
      [cod_lvl [cod_lvl . I32.suc]]
      [renaming [renaming . I32Map.set cod_lvl [dom_env . Env.lvl]]]
    ]]
  ]]]

  [def-decl pub miller.invert [-> Cxt [Env Kind] [Spine Vy] [Maybe PartialRenaming]]]
  [def-impl miller.invert [\ cxt cod_env in_sp [do
    [let dom_lvl [in_sp . Spine.length]]
    [let cod_lvl [Env.lvl cod_env]]

    [let bounds_set [I32MutSet.create ,]]
    [let bounds [Array.create ,]]

    // * Extract all distinct bounds or fail
    [let should_fail [var false]]
    [in_sp . Spine.iter . Iter.foreach [\ elem [do
      [let elem [force_ty_weak [Cxt.eval cxt] elem]]
      [match elem
        [[Rigid [; [Bound l] l_sp]] [where [Bool.and [Spine.is_empty l_sp] [Bool.not [bounds_set . I32MutSet.has l]]]] [do
          [bounds_set . I32MutSet.add l]
          [bounds . Array.append l]
          continue
        ]]
        [_ [do
          [set should_fail true]
          break
        ]]
      ]
    ]]]
    [if-let [true] [get should_fail] [None]]

    // * Construct dom_env & Renaming
    [let dom_env [Array.create ,]]
    [let renaming [I32MutMap.create ,]]

    [let dom_i [var 0]]
    [bounds . Array.foreach_reversed [\ cod_l [do
      [let cod_i [cod_l . lvl_to_ix cod_lvl]]
      [let dom_l [get dom_i . lvl_to_ix dom_lvl]]

      [let kind [cod_env . Env.at cod_i]]
      [dom_env . Array.append kind]

      [renaming . I32MutMap.set cod_l dom_l]

      continue
    ]]]
    [let dom_env [Env.from_array dom_env]]

    // * Done
    [let pren [PartialRenaming.new* [dict
      [dom_env dom_env]
      [cod_lvl cod_lvl]
      [renaming [renaming . I32MutMap.freeze]]
    ]]]
    [Some pren]
  ]]]

  // Quicker than doing 'miller.invert <...> . Maybe.is_some'
  [def-decl pub miller.can_invert_spine [-> Cxt [Spine Vy] Bool]]
  [def-impl miller.can_invert_spine [\ cxt in_sp [do
    [let bounds_set [I32MutSet.create ,]]

    // * Check all are distinct bounds or fail
    [in_sp . Spine.iter . Iter.all [\ elem [do
      [let elem [force_ty_weak [Cxt.eval cxt] elem]]
      [match elem
        [[Rigid [; [Bound l] l_sp]]
          [where [Bool.and
            [Spine.is_empty l_sp]
            [Bool.not [bounds_set . I32MutSet.has l]]
          ]]
          [do
            [bounds_set . I32MutSet.add l]
            true
          ]
        ]
        [_ [do
          false
        ]]
      ]
    ]]]
  ]]]

  [def-decl pub miller.rename [-> Cxt TyMetaID PartialRenaming Vy [Maybe Ty]]]
  [def-impl miller.rename [\ cxt m_id in_pren in_ty [do
    [let* [go [\ [pren PartialRenaming] [ty Vy] [:: [type [Maybe Ty]] [do
      [let go_closure [\ [kind Kind] [c TyClosure] [do
        [let arg [Vy.bound [pren . PartialRenaming.dom_env . Env.lvl]]]
        [let t [eval_ty_closure [Cxt.eval cxt] c arg]]
        [go [PartialRenaming.lift kind pren] t]
      ]]]

      [let ty [force_ty_weak [Cxt.eval cxt] ty]]
      [match ty
        [[Rigid [; head sp]] [do
          [let head [match head
            [[Bound l] [do
              [let i [l . lvl_to_ix [pren . PartialRenaming.dom_env . Env.lvl]]]
              [Ty.Var i]
            ]]
            [[Class uid] [Ty.Class uid]]
            [[Known k] [Ty.Known k]]
          ]]
          [try sp [sp . Spine.traverse [go pren]]]
          [pure [Ty.app_spine head sp]]
        ]]
        [[Flex [; m_id* sp]] [do
          // Occurence check
          [if-let [true] [== m_id m_id*] [None]]
          [try sp [sp . Spine.traverse [go pren]]]
          [pure [Ty.app_spine [Ty.Meta m_id*] sp]]
        ]]
        [[Arr [; icit dom cod]] [do
          [try dom [go pren dom]]
          [try cod [go pren cod]]
          [pure [Ty.Arr icit dom cod]]
        ]]
        [[Forall [; kind c]] [go_closure kind c]]
        [[Lam [; kind c]] [go_closure kind c]]
        [[Tuple elems] [do
          [try elems [elems . Array.traverse [go pren]]]
          [pure [Ty.Tuple elems]]
        ]]
        [[Dict d] [do
          [try d [d . StringMap.traverse [go pren]]]
          [pure [Ty.Dict d]]
        ]]
      ]
    ]]]]]
    [go in_pren in_ty]
  ]]]
]