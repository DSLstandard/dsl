[file
  [import "base.dsl"]
  [import "wlp.dsl"]

  // Represents an URI or a filepath or anything that identifies the address of
  // some source.
  //
  // Dim stands for "Dimension".
  [tydef Dim * String]

  // * Position

  [class-decl Pos]
  [class-struct Pos
    // Both are 0-based.
    [field line I32]
    [field col I32]
  ]

  [def-decl pub Pos.start_of_file Pos]
  [def-impl Pos.start_of_file [Pos.new 0 0]]

  [def-decl pub Pos.< [-> Pos Pos Bool]]
  [def-impl Pos.< [\ a b [do
    [if [[a . Pos.line] . < [b . Pos.line]]
      true
      [if [[a . Pos.line] . == [b . Pos.line]]
        [[a . Pos.col] . < [b . Pos.col]]
        false
      ]
    ]
  ]]]

  [def-decl pub Pos.> [-> Pos Pos Bool]]
  [def-impl Pos.> [\ a b [Pos.< b a]]]

  [def-decl pub Pos.== [-> Pos Pos Bool]]
  [def-impl Pos.== [\ a b
    [Bool.and
      [[a . Pos.line] . == [b . Pos.line]]
      [[a . Pos.col] . == [b . Pos.col]]
    ]
  ]]

  [def-decl pub Pos.min [-> Pos Pos Pos]]
  [def-impl Pos.min [\ a b [if [Pos.< a b] a b]]]

  [def-decl pub Pos.max [-> Pos Pos Pos]]
  [def-impl Pos.max [\ a b [if [Pos.< a b] b a]]]

  [def-decl pub Pos.next_char [-> Pos Pos]]
  [def-impl Pos.next_char [\ pos
    [pos . Pos.=col [pos . Pos.col . I32.suc]]
  ]]

  [def-decl pub Pos.next_line [-> Pos Pos]]
  [def-impl Pos.next_line [\ pos
    [Pos.new [pos . Pos.line . I32.+ 1] 0]
  ]]

  [def-decl pub Pos.advance_by_char [-> Pos Char Pos]]
  [def-impl Pos.advance_by_char [\ pos ch
    [if [Char.== ch '\n']
      [pos . Pos.next_line]
      [pos . Pos.next_char]
    ]
  ]]

  [def-decl pub Pos.advance_by_string [-> Pos String Pos]]
  [def-impl Pos.advance_by_string [\ pos str [do
    [let pos [var pos]]
    [str . String.foreach [\ ch [do
      [set pos [Pos.advance_by_char [get pos] ch]]
      continue
    ]]]
    [get pos]
  ]]]

  [def-decl pub Pos.from_pair [-> [; I32 I32] Pos]]
  [def-impl Pos.from_pair [\ input
    [match input
      [[; line col] [Pos.new line col]]
    ]
  ]]

  [def-decl pub auto Pos.Doc.Pretty [Doc.Pretty Pos]]
  [def-impl Pos.Doc.Pretty [Doc.Pretty.new [\ [type N] self
    // NOTE: Line and col are 0-based, but it is more natural to show them as
    // 1-based. Some editors like VSCode also interpret on-terminal file URI
    // location position text as 1-based.
    [Doc.parens [Doc.of [self . Pos.line . I32.suc] . <> [Doc.of ","] . <> [Doc.of [self . Pos.col . I32.suc]]]]
  ]]]

  // * Span

  [class-decl Span]
  [class-struct Span
    [field start Pos]
    [field stop Pos]
  ]

  [def-decl pub Span.union [-> Span Span Span]]
  [def-impl Span.union [\ a b
    [Span.new
      [Pos.min [a . Span.start] [b . Span.start]]
      [Pos.max [a . Span.stop] [b . Span.stop]]
    ]
  ]]

  [def-decl pub Span.of_source [-> String Span]]
  [def-impl Span.of_source [\ source
    [Span.new Pos.start_of_file [Pos.start_of_file . Pos.advance_by_string source]]
  ]]

  [def-decl pub auto Span.Doc.Pretty [Doc.Pretty Span]]
  [def-impl Span.Doc.Pretty [Doc.Pretty.new [\ [type N] self
    [Doc.of [self . Span.start] . <> [Doc.of "-"] . <> [Doc.of [self . Span.stop]]]
  ]]]

  // * Loc

  [class-decl Loc]
  [class-struct Loc
    [field dim Dim]
    [field span Span]
  ]

  [def-decl pub auto Loc.Doc.Pretty [Doc.Pretty Loc]]
  [def-impl Loc.Doc.Pretty [Doc.Pretty.new [\ [type N] self
    [[Doc.of [self . Loc.dim]] . <> [Doc.of ":"] . <> [Doc.of [self . Loc.span]]]
  ]]]

  [def-decl pub auto Loc.Debug [Debug Loc]]
  [def-impl Loc.Debug [Debug.new [\ self out
    [self . Doc.of . Doc.render_to out]
  ]]]

  [def-decl pub Loc.union [-> Loc Loc Loc]]
  [def-impl Loc.union [\ a b [do
    [Loc.new
      [a . Loc.dim]
      [Span.union [a . Loc.span] [b . Loc.span]]
    ]
  ]]]

  [def-decl pub Loc.of_source [-> Dim String Loc]]
  [def-impl Loc.of_source [\ dim source
    [Loc.new dim [Span.of_source source]]
  ]]
]