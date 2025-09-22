[file
  [import "base.dsl"]
  [import "dsl/types.dsl"]

  // * Kind utils

  [def-decl pub Kind.unfold_arr [-> Kind [; [Array Kind] Kind]]]
  [def-impl Kind.unfold_arr [\ in_kind [do
    [let doms [Array.create ,]]
    [let*
      [go [\ [kind Kind] [
        [match kind
          [[Arr [; dom cod]] [do
            [doms . Array.append dom]
            [go cod]
          ]]
          [cod [
            [; doms cod]
          ]]
        ]
      ]]]
    ]
    [go in_kind]
  ]]]

  [def-decl pub Kind.fold_arr [-> [Iter Kind] Kind Kind]]
  [def-impl Kind.fold_arr [\ doms cod [do
    [doms . Iter.foldr cod Kind.Arr]
  ]]]

  [def-decl pub Kind.is_arity_known [-> Kind Bool]]
  [def-impl Kind.is_arity_known [\ kind
    [match kind
      [[Arr [; _ cod]] [Kind.is_arity_known cod]]
      [[Meta] false]
      [_ true]
    ]
  ]]

  [def-decl pub Kind.drop_arr [-> Kind I32 Kind]]
  [def-impl Kind.drop_arr [\ in_kind num_doms [do
    [let kind [var in_kind]]
    [loop<n num_doms [\ _ [do
      [match [get kind]
        [[Arr [; _ cod]] [set kind cod]]
        [_ [panic "Kind.drop_arr: not enough arrs to drop"]]
      ]
      continue
    ]]]
    [get kind]
  ]]]

  // * Ty scope

  [tydef TyScope * [Scope Kind]]

  [def-decl pub TyScope.fold_as_kind_arr [-> TyScope Kind Kind]]
  [def-impl TyScope.fold_as_kind_arr [\ scope in_cod [do
    [let cod [var in_cod]]
    [scope . Scope.iter_elems . Iter.foreach [\ kind [do
      [set cod [Kind.Arr kind [get cod]]]
      continue
    ]]]
    [get cod]
  ]]]

  [def-decl pub TyScope.fold_as_forall [-> TyScope Ty Ty]]
  [def-impl TyScope.fold_as_forall [\ scope in_cod [do
    [let cod [var in_cod]]
    [scope . Scope.iter_elems . Iter.foreach [\ kind [do
      [set cod [Ty.Forall kind [get cod]]]
      continue
    ]]]
    [get cod]
  ]]]

  // * Ty utils

  [def-decl pub Ty.app_spine [-> Ty [Spine Ty] Ty]]
  [def-impl Ty.app_spine [\ f sp [do
    [let f [var f]]
    [sp . Spine.iter . Iter.foreach [\ x [do
      [set f [Ty.App [get f] x]]
      continue
    ]]]
    [get f]
  ]]]

  [def-decl pub Ty.unfold_app [-> Ty [; Ty [Array Ty]]]]
  [def-impl Ty.unfold_app [\ in_ty [do
    [let xs [Array.create ,]]
    [let*
      [go [\ [ty Ty] [
        [match ty
          [[App [; f x]] [do
            [xs . Array.append x]
            [go f]
          ]]
          [f [do
            [xs . Array.reverse_inplace]
            [; f xs]
          ]]
        ]
      ]]]
    ]
    [go in_ty]
  ]]]

  [def-decl pub Ty.unfold_arr* [-> Ty [; [Array [; Icit Ty]] Ty]]]
  [def-impl Ty.unfold_arr* [\ in_ty [do
    [let doms [Array.create ,]]
    [let*
      [go [\ [ty Ty] [
        [match ty
          [[Arr [; icit dom cod]] [do
            [doms . Array.append [; icit dom]]
            [go cod]
          ]]
          [cod [
            [; doms cod]
          ]]
        ]
      ]]]
    ]
    [go in_ty]
  ]]]

  [def-decl pub Ty.arr [-> Ty Ty Ty]]
  [def-impl Ty.arr [\ dom cod [Ty.Arr Icit.Expl dom cod]]]

  [def-decl pub Ty.fold_expl_arr [-> [Iter Ty] Ty Ty]]
  [def-impl Ty.fold_expl_arr [\ doms in_cod [do
    [doms . Iter.foldr in_cod [Ty.Arr Icit.Expl]]
  ]]]

  [def-decl pub Ty.fold_lams_by_env [-> [Env Kind] Ty Ty]]
  [def-impl Ty.fold_lams_by_env [\ env in_cod [do
    [let cod [var in_cod]]
    [env . Env.iter . Iter.foreach [\ kind [do
      [set cod [Ty.Lam kind [get cod]]]
      continue
    ]]]
    [get cod]
  ]]]

  // * Vy constructors

  [def-decl pub Vy.meta [-> TyMetaID Vy]]
  [def-impl Vy.meta [\ m [Vy.Flex m [Spine.nil]]]]

  [def-decl pub Vy.bound [-> TyLvl Vy]]
  [def-impl Vy.bound [\ l [Vy.Rigid [VyHead.Bound l] [Spine.nil]]]]

  [def-decl pub Vy.known [-> KnownType Vy]]
  [def-impl Vy.known [\ k [Vy.Rigid [VyHead.Known k] [Spine.nil]]]]

  [def-decl pub Vy.class [-> UID Vy]]
  [def-impl Vy.class [\ u [Vy.Rigid [VyHead.Class u] [Spine.nil]]]]

  [def-decl pub Vy.unit Vy]
  [def-impl Vy.unit [Vy.Tuple [Array.create ,]]]

  [def-decl pub Vy.vec [-> Vy Vy]]
  [def-impl Vy.vec [\ elem_ty [Vy.Rigid [VyHead.Known KnownType.Vec] [Spine.one elem_ty]]]]

  [def-decl pub Vy.bool Vy]
  [def-impl Vy.bool [Vy.Rigid [VyHead.Known KnownType.Bool] [Spine.nil]]]

  [def-decl pub Vy.string Vy]
  [def-impl Vy.string [Vy.Rigid [VyHead.Known KnownType.String] [Spine.nil]]]

  [def-decl pub Vy.char Vy]
  [def-impl Vy.char [Vy.Rigid [VyHead.Known KnownType.Char] [Spine.nil]]]

  [def-decl pub Vy.i32 Vy]
  [def-impl Vy.i32 [Vy.Rigid [VyHead.Known KnownType.I32] [Spine.nil]]]

  // * Known type utils

  [def-decl KnownType.all [Vec KnownType]]
  [def-impl KnownType.all [vec
    KnownType.U8
    KnownType.U16
    KnownType.U32
    KnownType.U64
    KnownType.I8
    KnownType.I16
    KnownType.I32
    KnownType.I64
    KnownType.Int
    KnownType.String
    KnownType.Bool
    KnownType.Char
    KnownType.Vec
    KnownType.Bytes
  ]]

  [def-decl pub KnownType.name [-> KnownType Name]]
  [def-impl KnownType.name [\ known [match known
    [[U8] "U8"]
    [[U16] "U16"]
    [[U32] "U32"]
    [[U64] "U64"]
    [[UInt] "UInt"]

    [[I8] "I8"]
    [[I16] "I16"]
    [[I32] "I32"]
    [[I64] "I64"]
    [[Int] "Int"]

    [[String] "String"]
    [[Bool] "Bool"]
    [[Char] "Char"]
    [[Vec] "Vec"]
    [[Bytes] "Bytes"]
  ]]]

  [def-decl KnownType.name_to_known_map [StringMap KnownType]]
  [def-impl KnownType.name_to_known_map [do
    [let map [StringMutMap.create ,]]
    [KnownType.all . Vec.foreach [\ known [do
      [map . StringMutMap.set [known . KnownType.name] known]
      continue
    ]]]
    [map . StringMutMap.freeze]
  ]]

  [def-decl pub KnownType.from_name? [-> String [Maybe KnownType]]]
  [def-impl KnownType.from_name? [\ name [do
    [KnownType.name_to_known_map . StringMap.get? name]
  ]]]

  [def-decl pub KnownType.kind [-> KnownType Kind]]
  [def-impl KnownType.kind [\ known [match known
    [[U8] Kind.Star]
    [[U16] Kind.Star]
    [[U32] Kind.Star]
    [[U64] Kind.Star]
    [[UInt] Kind.Star]

    [[I8] Kind.Star]
    [[I16] Kind.Star]
    [[I32] Kind.Star]
    [[I64] Kind.Star]
    [[Int] Kind.Star]

    [[String] Kind.Star]
    [[Bool] Kind.Star]
    [[Char] Kind.Star]
    [[Bytes] Kind.Star]
    [[Vec] [Kind.Arr Kind.Star Kind.Star]]
  ]]]

  // * More type utils

  [def-decl pub Spine.of_ty_bounds [-> Lvl [Spine Vy]]]
  [def-impl Spine.of_ty_bounds [\ lvl [do
    [let args [Array.create ,]]
    [loop<n lvl [\ l [do
      [args . Array.append [Vy.bound l]]
      continue
    ]]]
    [Spine.from_array args]
  ]]]

  // Like a quoted version of 'Spine.of_ty_bounds'
  [def-decl pub Spine.of_ty_vars [-> Lvl [Spine Ty]]]
  [def-impl Spine.of_ty_vars [\ lvl [do
    [let args [Array.create ,]]
    [loop<n_reversed lvl [\ i [do
      [args . Array.append [Ty.Var i]]
      continue
    ]]]
    [Spine.from_array args]
  ]]]

  // `Spine.of_tm_vars 3 = [Tm.Var 2, Tm.Var 1, Tm.Var 0]`
  [def-decl pub Spine.of_tm_vars [-> Lvl [Spine Tm]]]
  [def-impl Spine.of_tm_vars [\ lvl [do
    [let args [Array.create ,]]
    [loop<n_reversed lvl [\ i [do
      [args . Array.append [Tm.Var i]]
      continue
    ]]]
    [Spine.from_array args]
  ]]]

  [def-decl pub Env.of_ty_bounds [-> Lvl [Env Vy]]]
  [def-impl Env.of_ty_bounds [\ lvl [do
    [let args [Array.create ,]]
    [loop<n_reversed lvl [\ l [do
      [args . Array.append [Vy.bound l]]
      continue
    ]]]
    [Env.from_array args]
  ]]]

  [def-decl pub Env.of_debug_name_env [-> Lvl [Env Name]]]
  [def-impl Env.of_debug_name_env [\ lvl [do
    [let args [Array.create ,]]
    [loop<n_reversed lvl [\ l [do
      [args . Array.append ["#" . <> [I32.to_str l]]]
      continue
    ]]]
    [Env.from_array args]
  ]]]

  // * Tm scope

  [tydef TmScope * [Scope Vy]]

  // * Tm utils

  // Like 'Tm.Var' but with a check for negative indices.
  [def-decl pub Tm.var [-> TmIx Tm]]
  [def-impl Tm.var [\ i [do
    // A very common error during compiler development, worth a
    [when [i . < 0] [\ _ [do
      [panic ["Tm.var: got bad variable index" . <> [I32.to_str i]]]
    ]]]
    [Tm.Var i]
  ]]]

  // Makes a Tm.App but tail call is marked as false for convenience.
  [def-decl pub Tm.app [-> Tm Tm Tm]]
  [def-impl Tm.app [\ f x [Tm.App false f x]]]

  [def-decl pub Tm.app_spine [-> Tm [Spine Tm] Tm]]
  [def-impl Tm.app_spine [\ f sp [do
    [let f [var f]]
    [sp . Spine.iter . Iter.foreach [\ x [do
      [set f [Tm.app [get f] x]]
      continue
    ]]]
    [get f]
  ]]]

  // `Tm.fold_lam 3 body = [\ x1 x2 x3 body]`
  [def-decl pub Tm.fold_lam [-> TmLvl Tm Tm]]
  [def-impl Tm.fold_lam [\ lvl in_body [do
    [fn.repeat lvl Tm.Lam in_body]
  ]]]

  [def-decl pub Tm.count_lam [-> Tm I32]]
  [def-impl Tm.count_lam [\ tm [do
    [let count [var 0]]
    [let tm [var tm]]
    [loop [\ _ [do
      [else-let [Lam body] [get tm] break]
      [update count [I32.+ 1]]
      [set tm body]
      continue
    ]]]
    [get count]
  ]]]
]
