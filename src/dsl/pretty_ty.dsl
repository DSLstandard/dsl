[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "dsl/pretty_kind.dsl"]
  [import "wlp.dsl"]

  [def-decl pretty_uid [-> [type N] Database UID [Doc N]]]
  [def-impl pretty_uid [\ [type N] db uid [do
    [db . Database.get_entry uid . XEntry.name . Doc.of]
  ]]]

  [def-decl pub pretty_ty [-> [type N] Database [Env Name] Ty [Doc N]]]
  [def-impl pretty_ty [\ [type N] db env in_ty [do
    [match in_ty
      [[Meta m] [do
        [[Doc.of "?"] . <> [Doc.of m]]
      ]]

      [[Var i] [do
        [Doc.of [env . Env.at i]]
      ]]
      [[Known known] [do
        [known . KnownType.name . Doc.of]
      ]]

      [[Def uid] [pretty_uid db uid]]
      [[Class uid] [pretty_uid db uid]]

      [[App] [do
        [let= [; f xs] [Ty.unfold_app in_ty]]

        [let docs [Foldable.mconcat [vec
          [Iter.one [pretty_ty [type N] db env f]]
          [xs . Array.iter . map [pretty_ty db env]]
        ]]]
        [pretty_slist docs]
      ]]
      [[Arr] [do
        [let= [; doms cod] [Ty.unfold_arr* in_ty]]

        [let docs [Foldable.mconcat [vec
          [Iter.one [Doc.of [type N] "->"]]
          [doms . Array.iter . map [\ dom [do
            [let= [; icit ty] [dom]]
            [let ty_doc [pretty_ty db env ty]]
            [match icit
              [[Expl] ty_doc]
              [[Auto] [pretty_slist [[vec [Doc.of "auto"] ty_doc] . Vec.iter]]]
            ]
          ]]]
          [Iter.one [pretty_ty db env cod]]
        ]]]
        [pretty_slist docs]
      ]]

      [[Tuple elems] [do
        [if-let [true] [elems . Array.is_empty] [do
          [Doc.of ',']
        ]]

        [let docs [Foldable.mconcat [vec
          [Iter.one [Doc.of [type N] ";"]]
          [elems . Array.iter . map [pretty_ty db env]]
        ]]]
        [pretty_slist docs]
      ]]
      [[Dict entries] [do
        [let docs [Foldable.mconcat [vec
          [Iter.one [Doc.of [type N] "Dict"]]
          [entries . StringMap.iter . map [\ entry [do
            [let= [; n t] entry]
            [let n_doc [Doc.of n]]
            [let t_doc [pretty_ty db env t]]
            [pretty_slist [[vec n_doc t_doc] . Vec.iter]]
          ]]]
        ]]]
        [pretty_slist docs]
      ]]
      [[Forall [; kind cod]] [do
        [let n ["#" . <> [env . Env.lvl . I32.to_str]]]
        [Foldable.mconcat [vec
          [Doc.of "∀("]
          [Doc.of n]
          [Doc.of ": "]
          [pretty_kind kind]
          [Doc.of "). "]
          [pretty_ty db [env . Env.push n] cod]
        ]]
      ]]
      [[Lam [; kind cod]] [do
        [let n ["#" . <> [env . Env.lvl . I32.to_str]]]
        [Foldable.mconcat [vec
          [Doc.of "λ("]
          [Doc.of n]
          [Doc.of ": "]
          [pretty_kind kind]
          [Doc.of "). "]
          [pretty_ty db [env . Env.push n] cod]
        ]]
      ]]
    ]
  ]]]
]