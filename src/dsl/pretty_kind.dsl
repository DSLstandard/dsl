[file
  [import "base.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "wlp.dsl"]

  [def-decl pub pretty_kind [-> [type N] Kind [Doc N]]]
  [def-impl pretty_kind [\ [type N] kind
    [match kind
      [[Star] [do
        [Doc.of '*']
      ]]
      [[Meta m] [do
        [[Doc.of "?"] . <> [Doc.of [I32.to_str m]]]
      ]]
      [[Arr] [do
        [let= [; doms cod] [kind . Kind.unfold_arr]]
        [pretty_slist [
          Iter.chain [Array.iter doms] [Iter.one cod]
          . Iter.map [pretty_kind]
          . Iter.prepend [Doc.of "->"]
        ]]
      ]]
    ]
  ]]
]