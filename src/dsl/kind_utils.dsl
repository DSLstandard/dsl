[file
  [import "base.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/cxt.dsl"]

  // Find all meta variables appearing in a kind and return them as a set.
  [def-decl pub Kind.scan_meta_ids [-> Kind I32MutSet]]
  [def-impl Kind.scan_meta_ids [\ self [do
    [let set [I32MutSet.create ,]]

    [let*
      [go [\ [kind Kind] [do
        [match kind
          [[Star] ,]
          [[Arr [; dom cod]] [do
            [go dom]
            [go cod]
          ]]
          [[Meta m] [do
            [set . I32MutSet.add m]
          ]]
        ]
      ]]]
    ]
    [go self]

    set
  ]]]
]