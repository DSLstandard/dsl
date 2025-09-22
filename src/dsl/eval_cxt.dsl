[file
  [import "base.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/meta_store.dsl"]

  [class-decl EvalCxt]
  [class-struct EvalCxt
    [field db Database]
    [field metas MetaStore]
  ]
]