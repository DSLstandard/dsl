[file
  [import "base.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/location.dsl"]

  // * Misc. types

  [tydef Ix * I32] // Annotates either a Tm/Ty (or other) Ix
  [tydef Lvl * I32] // Annotates either a Tm/Ty (or other) Lvl

  [def-decl pub lvl_to_ix [-> Lvl Lvl Ix]]
  [def-impl lvl_to_ix [\ l lvl [lvl . I32.- l . I32.- 1]]]

  [def-decl pub ix_to_lvl [-> Ix Lvl Lvl]]
  [def-impl ix_to_lvl [\ i lvl [lvl . I32.- i . I32.- 1]]]

  [tydef Name * String]
  [tydef MatchClauseID * I32]
  [tydef MemberID * I32]

  // This is unique within the whole compiler
  [tydef UID * I32]

  // * Scope

  // Acts like a List with more efficient runtime (e.g., cached length)
  //
  // Tailored for term/type scopes keyed by names.
  [class-decl Scope [A *]]
  [class-struct Scope
    [field entries [Seq [; Name A]]]
    [field name_map [StringMap [; Lvl A]]]
  ]

  [def-decl pub Scope.nil [-> [type A] [Scope A]]]
  [def-impl Scope.nil [\ [type A] [Scope.new [Seq.nil] [StringMap.empty]]]]

  [def-decl pub Scope.lvl [-> [type A] [Scope A] Lvl]]
  [def-impl Scope.lvl [\ [type A] self [self . Scope.entries . Seq.length]]]

  [def-decl pub Scope.push [-> [type A] [Scope A] Name A [Scope A]]]
  [def-impl Scope.push [\ [type A] self name value
    [Scope.new
      [self . Scope.entries . Seq.cons [; name value]]
      [self . Scope.name_map . StringMap.set name [; [self . Scope.lvl] value]]
    ]
  ]]

  [def-decl pub Scope.one [-> [type A] Name A [Scope A]]]
  [def-impl Scope.one [\ [type A] name entry
    [Scope.nil . Scope.push name entry]
  ]]

  [def-decl pub Scope.extend [-> [type A] [Scope A] [Scope A] [Scope A]]]
  [def-impl Scope.extend [\ [type A] self sub [do
    [let self_lvl [self . Scope.lvl]]
    [Scope.new
      [[sub . Scope.entries] . <> [self . Scope.entries]]
      [StringMap.merge_with_key
        [sub . Scope.name_map]
        [self . Scope.name_map]
        [\ key these
          [Some [match these
            [[L a] [a . Pair.map_fst [I32.+ self_lvl]]]
            [[R b] b]
            [[T [; a _]] [a . Pair.map_fst [I32.+ self_lvl]]]
          ]]
        ]
      ]
    ]
  ]]]

  // If iterator yields [("a", val_a), ("b", val_b), ("c", val_c)], then the
  // resulting Scope behaves as 'a = val_a, b = val_b, c = val_c |- <body>'.
  //
  // Named '_reversed' as the order is opposite of 'Scope.iter_elems'.
  [def-decl pub Scope.from_iter_reversed [-> [type A] [Iter [; Name A]] [Scope A]]]
  [def-impl Scope.from_iter_reversed [\ [type A] iter [do
    [let entries [Array.create ,]]
    [let name_map [StringMutMap.create ,]]

    [iter . Iter.foreach [\ entry [do
      [let= [; name val] entry]
      [name_map . StringMutMap.set name [; [Array.length entries] val]]
      [entries . Array.append [; name val]]
      continue
    ]]]

    [Scope.new* [dict
      [entries [entries . Array.iter_reversed . Seq.from_iter]]
      [name_map [name_map . StringMutMap.freeze]]
    ]]
  ]]]

  [def-decl pub Scope.resolve [-> [type A] [Scope A] Name [Maybe [; I32 A]]]]
  [def-impl Scope.resolve [\ [type A] self name [do
    [try res [self . Scope.name_map . StringMap.get? name]]
    [pure [res . Pair.map_fst [\ l [l . lvl_to_ix [Scope.lvl self]]]]]
  ]]]

  [def-decl pub Scope.resolve_ix [-> [type A] [Scope A] Name [Maybe I32]]]
  [def-impl Scope.resolve_ix [\ [type A] self name [do
    [self . Scope.resolve name . map [Pair.fst]]
  ]]]

  [def-decl pub Scope.at [-> [type A] [Scope A] Ix A]]
  [def-impl Scope.at [\ [type A] self i
    [self . Scope.entries . Seq.at i . Pair.snd]
  ]]

  // Used for iterating over all entries in the scope, from index 0 to index (Scope.lvl - 1).
  [def-decl pub Scope.iter_elems [-> [type A] [Scope A] [Iter A]]]
  [def-impl Scope.iter_elems [\ [type A] self [do
    [self . Scope.entries . Seq.iter . Iter.map [Pair.snd]]
  ]]]

  // Effectively disables name resolution by making all names resolve to None.
  [def-decl pub Scope.nullify_resolve [-> [type A] [Scope A] [Scope A]]]
  [def-impl Scope.nullify_resolve [\ [type A] self [do
    [Scope.new
      [self . Scope.entries]
      [StringMap.empty]
    ]
  ]]]

  // * Env

  [class-decl Env [A *]]
  [class-struct Env
    [field seq [Seq A]]
  ]

  [def-decl pub Env.nil [-> [type A] [Env A]]]
  [def-impl Env.nil [\ [type A] [Env.new [Seq.nil]]]]

  [def-decl pub Env.lvl [-> [type A] [Env A] Lvl]]
  [def-impl Env.lvl [\ [type A] self [self . Env.seq . Seq.length]]]

  [def-decl pub Env.push [-> [type A] [Env A] A [Env A]]]
  [def-impl Env.push [\ [type A] self entry
    [Env.new [self . Env.seq . Seq.cons entry]]
  ]]

  [def-decl pub Env.at? [-> [type A] [Env A] Ix [Maybe A]]]
  [def-impl Env.at? [\ [type A] self i [do
    [self . Env.seq . Seq.at? i]
  ]]]

  [def-decl pub Env.at [-> [type A] [Env A] Ix A]]
  [def-impl Env.at [\ [type A] self i
    [self . Env.at? i . Maybe.expect* [\ _
      ["Env.at: index "
        . <> [i . I32.to_str]
        . <> " out of bounds (env lvl = "
        . <> [Env.lvl self . I32.to_str]
        . <> ")"]
    ]]
  ]]

  [def-decl pub Env.one [-> [type A] A [Env A]]]
  [def-impl Env.one [\ [type A] entry
    [Env.new [Seq.one entry]] 
  ]]

  [def-decl pub Env.extend [-> [type A] [Env A] [Env A] [Env A]]]
  [def-impl Env.extend [\ [type A] self sub [do
    [Env.new [[sub . Env.seq] . <> [self . Env.seq]]]
  ]]]

  [def-decl pub Env.from_iter [-> [type A] [Iter A] [Env A]]]
  [def-impl Env.from_iter [\ [type A] iter
    [Env.new [Seq.from_iter iter]]
  ]]

  [def-decl pub Env.from_array [-> [type A] [Array A] [Env A]]]
  [def-impl Env.from_array [\ [type A] arr
    [Env.from_iter [arr . Array.iter]]
  ]]

  [def-decl pub Env.iter [-> [type A] [Env A] [Iter A]]]
  [def-impl Env.iter [\ [type A] self [do
    [self . Env.seq . Seq.iter]
  ]]]

  // * Spine

  [class-decl Spine [A *]]
  [class-struct Spine
    // If 'f a b c', then a spine for 'f' would be stored as 'Seq.fromVec [vec a b]'.
    [field seq [Seq A]]
  ]

  [def-decl pub Spine.of_seq [-> [type A] [Seq A] [Spine A]]]
  [def-impl Spine.of_seq [\ [type A] [Spine.new]]]

  [def-decl pub Spine.nil [-> [type A] [Spine A]]]
  [def-impl Spine.nil [\ [type A] [Spine.new [Seq.nil]]]]

  [def-decl pub Spine.is_empty [-> [type A] [Spine A] Bool]]
  [def-impl Spine.is_empty [\ [type A] self
    [self . Spine.seq . Seq.is_empty]
  ]]

  [def-decl pub Spine.<> [-> [type A] [Spine A] [Spine A] [Spine A]]]
  [def-impl Spine.<> [\ [type A] self other
    [Spine.new [[self . Spine.seq] . <> [other . Spine.seq]]]
  ]]

  [def-decl pub Spine.snoc [-> [type A] [Spine A] A [Spine A]]]
  [def-impl Spine.snoc [\ [type A] self entry
    [Spine.new [self . Spine.seq . Seq.snoc entry]]
  ]]

  [def-decl pub Spine.from_array [-> [type A] [Array A] [Spine A]]]
  [def-impl Spine.from_array [\ [type A] arr
    [Spine.new [Seq.from_array arr]]
  ]]

  [def-decl pub Spine.to_array [-> [type A] [Spine A] [Array A]]]
  [def-impl Spine.to_array [\ [type A] self
    [self . Spine.seq . Seq.to_array]
  ]]

  [def-decl pub Spine.map [-> [type A] [type B] [Spine A] [-> A B] [Spine B]]]
  [def-impl Spine.map [\ [type A] [type B] self f
    [Spine.new [self . Spine.seq . Seq.map f]]
  ]]

  [def-decl pub Spine.traverse [-> [type A] [type B] [type M] [auto [Monad M]] [Spine A] [-> A [M B]] [M [Spine B]]]]
  [def-impl Spine.traverse [\ [type A] [type B] [type M] [auto M.Monad] self f
    [self . Spine.seq . Seq.traverse f . map [Spine.new]]
  ]]

  [def-decl pub Spine.length [-> [type A] [Spine A] I32]]
  [def-impl Spine.length [\ [type A] self
    [self . Spine.seq . Seq.length]
  ]]

  [def-decl pub Spine.at? [-> [type A] [Spine A] I32 [Maybe A]]]
  [def-impl Spine.at? [\ [type A] self i
    [self . Spine.seq . Seq.at? i]
  ]]

  [def-decl pub Spine.at [-> [type A] [Spine A] I32 A]]
  [def-impl Spine.at [\ [type A] self i
    [self . Spine.at? i . Maybe.expect* [\ _
      ["Spine.at: index "
        . <> [i . I32.to_str]
        . <> " out of bounds (length = "
        . <> [Spine.length self . I32.to_str]
        . <> ")"]
    ]]
  ]]

  [def-decl pub Spine.uncons [-> [type A] [Spine A] [Maybe [; A [Spine A]]]]]
  [def-impl Spine.uncons [\ [type A] self [do
    [try result [self . Spine.seq . Seq.uncons]]
    [let= [; x xs] result]
    [pure [; x [Spine.of_seq xs]]]
  ]]]

  [def-decl pub Spine.iter [-> [type A] [Spine A] [Iter A]]]
  [def-impl Spine.iter [\ [type A] self [do
    [self . Spine.seq . Seq.iter]
  ]]]

  [def-decl pub Spine.one [-> [type A] A [Spine A]]]
  [def-impl Spine.one [\ [type A] entry
    [Spine.new [Seq.one entry]]
  ]]

  [def-decl pub Spine.take [-> [type A] [Spine A] I32 [Spine A]]]
  [def-impl Spine.take [\ [type A] self len
    [Spine.new [self . Spine.seq . Seq.take len]]
  ]]

  // More Spine / Env / Scope functions

  [def-decl pub Scope.name_env [-> [type A] [Scope A] [Env Name]]]
  [def-impl Scope.name_env [\ [type A] self [do
    [Env.new [self . Scope.entries . Seq.map [Pair.fst]]]
  ]]]

  [def-decl pub Scope.elem_env [-> [type A] [Scope A] [Env A]]]
  [def-impl Scope.elem_env [\ [type A] self [do
    [Env.new [self . Scope.entries . Seq.map [Pair.snd]]]
  ]]]

  [def-decl pub Env.from_spine [-> [type A] [Spine A] [Env A]]]
  [def-impl Env.from_spine [\ [type A] sp [Env.new [sp . Spine.seq . Seq.reverse]]]]

  [def-decl pub Env.to_spine [-> [type A] [Env A] [Spine A]]]
  [def-impl Env.to_spine [\ [type A] self [Spine.new [self . Env.seq . Seq.reverse]]]]

  // * Kind level

  [tydef KindMetaID * I32]

  [class-decl Kind]
  [class-enum Kind
    [member Star ,]
    [member Arr [; Kind Kind]]
    [member Meta KindMetaID]
  ]

  // * Type level

  [class-decl KnownType]
  [class-enum KnownType
    [member U8 ,]
    [member U16 ,]
    [member U32 ,]
    [member U64 ,]
    [member UInt ,]

    [member I8 ,]
    [member I16 ,]
    [member I32 ,]
    [member I64 ,]
    [member Int ,]

    [member String ,]
    [member Bool ,]
    [member Char ,]
    [member Vec ,]
    [member Bytes ,]
  ]

  [def-decl pub auto KnownType.Eq [Eq KnownType]]
  [def-impl KnownType.Eq [Eq.new
    [\ lhs rhs
      [match [; lhs rhs]
        [[; [U8] [U8]] true]
        [[; [U16] [U16]] true]
        [[; [U32] [U32]] true]
        [[; [U64] [U64]] true]
        [[; [UInt] [UInt]] true]
        [[; [I8] [I8]] true]
        [[; [I16] [I16]] true]
        [[; [I32] [I32]] true]
        [[; [I64] [I64]] true]
        [[; [Int] [Int]] true]
        [[; [String] [String]] true]
        [[; [Bool] [Bool]] true]
        [[; [Char] [Char]] true]
        [[; [Vec] [Vec]] true]
        [[; [Bytes] [Bytes]] true]
        [_ false]
      ]
    ]
  ]]

  [tydef TyMetaID * I32]
  [tydef TyIx * Ix]
  [tydef TyLvl * Lvl]

  [class-decl Icit]
  [class-enum Icit
    [member Expl ,]
    [member Auto ,]
  ]

  [def-decl pub auto Icit.Eq [Eq Icit]]
  [def-impl Icit.Eq [Eq.new [\ lhs rhs
    [match [; lhs rhs]
      [[; [Expl] [Expl]] true]
      [[; [Auto] [Auto]] true]
      [_ false]
    ]
  ]]]

  [class-decl Ty]
  [class-enum Ty
    // Flex
    [member Meta TyMetaID]

    // Rigid
    [member Var TyIx]
    [member Known KnownType]
    [member Class UID]

    [member Def UID]
    [member App [; Ty Ty]]
    [member Tuple [Array Ty]]
    [member Dict [StringMap Ty]]

    [member Arr [; Icit Ty Ty]]
    [member Forall [; Kind Ty]]
    [member Lam [; Kind Ty]]
  ]

  [class-decl VyHead]
  [class-enum VyHead
    [member Bound TyLvl]
    [member Known KnownType]
    [member Class UID]
  ]

  [def-decl pub auto VyHead.Eq [Eq VyHead]]
  [def-impl VyHead.Eq [Eq.new [type VyHead]
    [\ lhs rhs
      [match [; lhs rhs]
        [[; [Bound a] [Bound b]] [a . == b]]
        [[; [Known a] [Known b]] [a . == b]]
        [[; [Class a] [Class b]] [a . == b]]
        [_ false]
      ]
    ]
  ]]

  [class-decl TyClosure]
  [class-decl Vy]

  [class-struct TyClosure
    [field env [Env Vy]]
    [field ty Ty]
  ]

  [class-enum Vy
    // NOTE: Flex & Rigid cannot have [Array Vy] because they should only be
    // updated immutably, but Tuple is fine because it won't be updated after
    // creation, and we want O(1) random access.
    [member Flex [; TyMetaID [Spine Vy]]]
    [member Rigid [; VyHead [Spine Vy]]]
    [member Tuple [Array Vy]]
    [member Dict [StringMap Vy]]
    [member Arr [; Icit Vy Vy]]
    [member Forall [; Kind TyClosure]]
    [member Lam [; Kind TyClosure]]
  ]

  // * Term level

  [class-decl Lit]
  [class-enum Lit
    [member Bool Bool]
    [member Char Char]
    [member I32 I32]
    [member String String]
  ]

  [def-decl pub auto Lit.Eq [Eq Lit]]
  [def-impl Lit.Eq [Eq.new [\ a b [do
    [match [; a b]
      [[; [Bool a] [Bool b]] [a . == b]]
      [[; [Char a] [Char b]] [a . == b]]
      [[; [I32 a] [I32 b]] [a . == b]]
      [[; [String a] [String b]] [a . == b]]
      [_ false]
    ]
  ]]]]

  [def-decl pub auto Lit.Debug [Debug Lit]]
  [def-impl Lit.Debug [Debug.new [\ self out [do
    [match self
      [[Bool b] [do
        [out . StringBuilder.append "[Bool "]
        [Debug.format_to b out]
        [out . StringBuilder.append "]"]
      ]]
      [[Char c] [do
        [out . StringBuilder.append "[Char "]
        [Debug.format_to c out]
        [out . StringBuilder.append "]"]
      ]]
      [[I32 i] [do
        [out . StringBuilder.append "[I32 "]
        [Debug.format_to i out]
        [out . StringBuilder.append "]"]
      ]]
      [[String s] [do
        [out . StringBuilder.append "[String "]
        [Debug.format_to s out]
        [out . StringBuilder.append "]"]
      ]]
    ]
  ]]]]

  [tydef TmIx * Ix]
  [tydef TmLvl * Lvl]
  [tydef PatLvl * Lvl]

  [class-decl Tm]
  [class-decl TmMatch.Node]

  [class-decl TmMatch.Leaf]
  [class-struct TmMatch.Leaf
    // The ID of the target clause to jump.
    [field target_clause_id MatchClauseID]

    // The pattern bound variables to get for the target clause.
    //
    // Its 'Env.lvl' must equal to that of the target clause's 'pat_env_lvl'.
    [field pat_env [Env PatLvl]]

    [field on_guard_fail [Maybe TmMatch.Node]]
  ]

  [class-decl TmMatch.Split.Class.Branch]
  [class-struct TmMatch.Split.Class.Branch
    // Which class member this branch wants?
    [field member_id MemberID]
    // The number of fields of the enum member. Used to determine how much
    // pat_env to be extended.
    [field next_node TmMatch.Node]
  ]

  [class-decl TmMatch.Split.Tuple.Branch]
  [class-struct TmMatch.Split.Tuple.Branch
    // The number of fields of the ;. Used to determine how much pat_env to
    // be extended.
    [field num_fields PatLvl]
    [field next_node TmMatch.Node]
  ]

  [class-decl TmMatch.Split.Bool.Branch]
  [class-struct TmMatch.Split.Bool.Branch
    // Expected boolean value
    [field value Bool]
    [field next_node TmMatch.Node]
  ]

  [class-decl TmMatch.Split]
  [class-enum TmMatch.Split
    [member Class [;
      // Target Class UID (exists to aid debugging; not used for actual pattern
      // matching at all)
      UID
      [Array TmMatch.Split.Class.Branch]
    ]]
    [member Tuple TmMatch.Split.Tuple.Branch]
    [member Bool [Array TmMatch.Split.Bool.Branch]]
  ]

  [class-enum TmMatch.Node
    // A leaf that corresponds to the "unhandled" case. Exists if a pattern
    // match is inexhaustive.
    [member Unhandled ,]

    [member Leaf TmMatch.Leaf]

    // Split on a pat bound variable.
    [member SplitOn [; PatLvl TmMatch.Split]]
  ]

  [class-decl TmMatch.Clause]
  [class-struct TmMatch.Clause
    // The ID of this clause.
    [field id MatchClauseID]

    // The total number of pattern variables of this clause.
    [field pat_env_lvl PatLvl]

    // The "RHS" of this clause. Scoped under 'pat_env_lvl' variables.
    [field body_tm Tm]

    // A guard is a boolean expression that must evaluate to true for the
    // destination to match. If the guard fails, the corresponding 'MatchLeaf'
    // will jump to its fail branch. Scoped under 'pat_env_lvl' variables.
    [field guard_tm [Maybe Tm]]
  ]

  [def-decl pub TmMatch.Clause.has_guard [-> TmMatch.Clause Bool]]
  [def-impl TmMatch.Clause.has_guard [\ self
    [self . TmMatch.Clause.guard_tm . Maybe.is_some]
  ]]

  [class-decl TmMatch.Tree]
  [class-struct TmMatch.Tree
    // The root scrutinee. Can be referred to by PatLvl = 0 in other TmMatch.*
    // constructs.
    [field root_scrut_tm Tm]

    // The root node of the match tree.
    [field root_node TmMatch.Node]

    // The user clauses. Each clause is indexed by MatchClauseID according to
    // the array index.
    [field clauses [Array TmMatch.Clause]]
  ]

  [class-enum Tm
    [member Var TmIx]
    [member Def UID]
    [member App [;
      // Is tail call?
      Bool
      // Function
      Tm
      // Argument
      Tm
    ]]
    [member Lam Tm]
    [member Let [; [Array Tm] Tm]]

    [member GetTupleIndex [; Tm I32]]
    [member SetTupleIndex [; Tm I32 Tm]]

    [member Lit Lit]
    [member Class [; UID MemberID Tm]]
    [member Tuple [Array Tm]]
    [member Vec [Array Tm]]

    // The keys must be unique.
    // The value terms must be executed in the order they are in the array.
    [member Dict [Array [; String Tm]]]
    [member GetDictKey [; Tm String]]

    [member If [; Tm Tm Tm]]
    [member Match TmMatch.Tree]
    [member Error ,]
  ]
]