[file
  [import "base.dsl"]

  [class-decl Namespace [A *]]
  [class-struct Namespace
    [field mapping [StringMutMap A]]
  ]

  [def-decl pub Namespace.create [-> [type A] , [Namespace A]]]
  [def-impl Namespace.create [\ [type A] _ [do
    [Namespace.new [StringMutMap.create ,]]
  ]]]

  [def-decl pub Namespace.set [-> [type A] [Namespace A] String A ,]]
  [def-impl Namespace.set [\ [type A] self key value [do
    [self . Namespace.mapping . StringMutMap.set key value]
  ]]]

  [def-decl pub Namespace.get? [-> [type A] [Namespace A] String [Maybe A]]]
  [def-impl Namespace.get? [\ [type A] self key [do
    [self . Namespace.mapping . StringMutMap.get? key]
  ]]]

  [def-decl pub Namespace.include [-> [type A] [Namespace A] [Namespace A] ,]]
  [def-impl Namespace.include [\ [type A] self other [do
    [self . Namespace.mapping . StringMutMap.update [other . Namespace.mapping]]
  ]]]
]