[file
  [import "base.dsl"]

  // NOTE: The logging implementation follows Haskell's 'Colog' library for
  // logging because its design is very good

  [class-decl LogAction [A *]]
  [class-struct LogAction 
    [field run [-> A ,]]
  ]

  [def-decl pub LogAction.cmap [-> [type A *] [type B *] [LogAction A] [-> B A] [LogAction B]]]
  [def-impl LogAction.cmap [\ [type A *] [type B *] self f
    [LogAction.new [\ b [do
      [let a [f b]]
      [self . LogAction.run a]
    ]]]
  ]]

  [def-decl pub LogAction.println [LogAction String]]
  [def-impl LogAction.println
    [LogAction.new [\ msg [do
      [sys.println msg]
    ]]]
  ]
]