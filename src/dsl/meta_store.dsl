[file
  [import "base.dsl"]
  [import "colog.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/types.dsl"]
  [import "wlp.dsl"]

  [class-decl MetaReason]
  [class-struct MetaReason
    // The associated expression
    [field expr Expr]
    // Description
    [field desc [Doc Void]]
  ]

  [class-decl KindMeta]
  [class-struct KindMeta
    [field id KindMetaID]
    [field solution [Maybe Kind]]
    [field reason MetaReason]
  ]

  [class-decl TyMeta]
  [class-struct TyMeta
    [field id TyMetaID]
    [field solution [Maybe Vy]]
    [field kind Kind]
    [field reason MetaReason]
  ]

  [class-decl MetaStore]
  [class-struct MetaStore
    // * Meta
    [field fresh_meta_id [-> , I32]]
    [field kind_meta_map [Mut [I32Map KindMeta]]] // NOTE: Cannot be I32MutMap, we may duplicate store.
    [field ty_meta_map [Mut [I32Map TyMeta]]] // NOTE: Cannot be I32MutMap, we may duplicate store.
  ]

  [def-decl pub MetaStore.create [-> , MetaStore]]
  [def-impl MetaStore.create [\ _ [do
    [let id_counter [var 0]]
    [MetaStore.new* [dict
      [fresh_meta_id [\ _ [do
        [let id [get id_counter]]
        [set id_counter [I32.suc id]]
        id
      ]]]
      [kind_meta_map [var [I32Map.empty]]]
      [ty_meta_map [var [I32Map.empty]]]
    ]]
  ]]]

  // Meta utils

  [def-decl pub MetaStore.create_kind_meta [-> MetaStore KindMeta ,]]
  [def-impl MetaStore.create_kind_meta [\ store meta [do
    [store . MetaStore.kind_meta_map
      . update [\ map [map . I32Map.set [meta . KindMeta.id] meta]]
    ]
  ]]]

  [def-decl pub MetaStore.create_ty_meta [-> MetaStore TyMeta ,]]
  [def-impl MetaStore.create_ty_meta [\ store meta [do
    [store . MetaStore.ty_meta_map
      . update [\ map [map . I32Map.set [meta . TyMeta.id] meta]]
    ]
  ]]]

  [def-decl pub MetaStore.get_kind_meta [-> MetaStore KindMetaID KindMeta]]
  [def-impl MetaStore.get_kind_meta [\ store id
    [store . MetaStore.kind_meta_map . get . I32Map.get id]
  ]]

  [def-decl pub MetaStore.get_ty_meta [-> MetaStore TyMetaID TyMeta]]
  [def-impl MetaStore.get_ty_meta [\ store id
    [store . MetaStore.ty_meta_map . get . I32Map.get id]
  ]]

  [def-decl pub MetaStore.set_kind_meta_solution [-> MetaStore KindMetaID Kind ,]]
  [def-impl MetaStore.set_kind_meta_solution [\ store id solution [do
    [let meta [store . MetaStore.get_kind_meta id]]
    [if-let [true] [meta . KindMeta.solution . Maybe.is_some] [do
      [panic ["Kind has already been solved: ?" . String.<> [id . I32.to_str]]]
    ]]

    [let meta [meta . KindMeta.=solution [Some solution]]]
    [store . MetaStore.kind_meta_map . update [\ map
      [map . I32Map.set id meta]
    ]]
  ]]]

  [def-decl pub MetaStore.set_ty_meta_solution [-> MetaStore TyMetaID Vy ,]]
  [def-impl MetaStore.set_ty_meta_solution [\ store id solution [do
    [let meta [store . MetaStore.get_ty_meta id]]
    [if-let [true] [meta . TyMeta.solution . Maybe.is_some] [do
      [panic ["Type has already been solved: ?" . String.<> [id . I32.to_str]]]
    ]]

    [let meta [meta . TyMeta.=solution [Some solution]]]
    [store . MetaStore.ty_meta_map . update [\ map
      [map . I32Map.set id meta]
    ]]
  ]]]
]
