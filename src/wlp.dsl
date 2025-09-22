[file
  [import "base.dsl"]

  // Wadler/Leijen Prettyprinter
  //
  // See https://hackage.haskell.org/package/prettyprinter

  // See
  // https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/src/Prettyprinter.Internal.html#Doc
  [class-decl Doc [N *]]
  [class-enum Doc
    // Nothing
    [member Empty ,]

    // Char: Must not be '\n'
    [member Char Char]

    // String: At least two characters long and does not contain '\n'
    [member String String]

    // Hard line break
    [member Line ,]

    // (<>) two documents together
    [member Cat [; [Doc N] [Doc N]]]

    // User annotation
    [member Annotated [; N [Doc N]]]

    // Our custom extension: a lazily computed Doc.
    [member Lazy [-> , [Doc N]]]
  ]

  [def-decl pub Doc.render_to [-> [type N] [Doc N] StringBuilder ,]]
  [def-impl Doc.render_to [\ [type N] self sb
    [match self
      [[Empty] ,]
      [[Char c] [sb . StringBuilder.append_char c]]
      [[String s] [sb . StringBuilder.append s]]
      [[Line] [sb . StringBuilder.append_char '\n']]
      [[Cat [; x y]] [do [x . Doc.render_to sb] [y . Doc.render_to sb]]]
      [[Annotated [; _ doc]] [doc . Doc.render_to sb]]
      [[Lazy action] [action , . Doc.render_to sb]]
    ]
  ]]

  [def-decl pub Doc.to_str [-> [type N] [Doc N] String]]
  [def-impl Doc.to_str [\ [type N] self [do
    [let sb [StringBuilder.create ,]]
    [self . Doc.render_to sb]
    [sb . StringBuilder.build]
  ]]]

  [def-decl pub Doc.empty [-> [type N] [Doc N]]]
  [def-impl Doc.empty [\ [type N] [Doc.Empty]]]

  [def-decl pub Doc.line [-> [type N] [Doc N]]]
  [def-impl Doc.line [\ [type N] [Doc.Line]]]

  [def-decl pub Doc.<> [-> [type N] [Doc N] [Doc N] [Doc N]]]
  [def-impl Doc.<> [\ [type N] x y [Doc.Cat x y]]]

  [def-decl pub Doc.<+> [-> [type N] [Doc N] [Doc N] [Doc N]]]
  [def-impl Doc.<+> [\ [type N] x y [x . Doc.<> [Doc.Char ' '] . Doc.<> y]]]

  [def-decl pub Doc.|+| [-> [type N] [Doc N] [Doc N] [Doc N]]]
  [def-impl Doc.|+| [\ [type N] x y [x . Doc.<> [Doc.Line] . Doc.<> y]]]

  [def-decl pub Doc.lazy [-> [type N] [-> , [Doc N]] [Doc N]]]
  [def-impl Doc.lazy [\ [type N] action [Doc.Lazy action]]]

  [def-decl pub auto Doc.Semigroup [-> [type N] [Semigroup [Doc N]]]]
  [def-impl Doc.Semigroup [\ [type N]
    [Semigroup.new [Doc.<>]]
  ]]

  [def-decl pub auto Doc.Neutral [-> [type N] [Neutral [Doc N]]]]
  [def-impl Doc.Neutral [\ [type N]
    [Neutral.new [Doc.empty]]
  ]]

  [def-decl pub Doc.hcat [-> [type N] [Iter [Doc N]] [Doc N]]]
  [def-impl Doc.hcat [\ [type N] docs
    [docs . Iter.foldl1? [Doc.<>] . Maybe.unwrap_or [Doc.empty]]
  ]]

  [def-decl pub Doc.hsep [-> [type N] [Iter [Doc N]] [Doc N]]]
  [def-impl Doc.hsep [\ [type N] docs
    [docs . Iter.foldl1? [Doc.<+>] . Maybe.unwrap_or [Doc.empty]]
  ]]

  [def-decl pub Doc.vcat [-> [type N] [Iter [Doc N]] [Doc N]]]
  [def-impl Doc.vcat [\ [type N] docs
    [docs . Iter.foldl1? [Doc.|+|] . Maybe.unwrap_or [Doc.empty]]
  ]]

  [def-decl pub Doc.mark [-> [type N] [Doc N] N [Doc N]]]
  [def-impl Doc.mark [\ [type N] doc ann
    [Doc.Annotated ann doc]
  ]]

  [def-decl pub Doc.intercalate [-> [type N] [Iter [Doc N]] [Doc N] [Doc N]]]
  [def-impl Doc.intercalate [\ [type N] f.docs delimiter
    [f.docs . Iter.foldl1? [\ acc doc [acc . Doc.<> delimiter . Doc.<> doc]] . Maybe.unwrap_or [Doc.empty]]
  ]]

  [def-decl pub Doc.surround [-> [type N] [Doc N] [Doc N] [Doc N] [Doc N]]]
  [def-impl Doc.surround [\ [type N] self left right
    [left . Doc.<> self . Doc.<> right]
  ]]

  [def-decl pub Doc.angles [-> [type N] [Doc N] [Doc N]]]
  [def-impl Doc.angles [\ [type N] self
    [self . Doc.surround [Doc.Char '<'] [Doc.Char '>']]
  ]]

  [def-decl pub Doc.parens [-> [type N] [Doc N] [Doc N]]]
  [def-impl Doc.parens [\ [type N] self
    [self . Doc.surround [Doc.Char '('] [Doc.Char ')']]
  ]]

  [def-decl pub Doc.brackets [-> [type N] [Doc N] [Doc N]]]
  [def-impl Doc.brackets [\ [type N] self
    [self . Doc.surround [Doc.Char '['] [Doc.Char ']']]
  ]]

  [def-decl pub Doc.braces [-> [type N] [Doc N] [Doc N]]]
  [def-impl Doc.braces [\ [type N] self
    [self . Doc.surround [Doc.Char '{'] [Doc.Char '}']]
  ]]

  [def-decl pub Doc.squotes [-> [type N] [Doc N] [Doc N]]]
  [def-impl Doc.squotes [\ [type N] self
    [self . Doc.surround [Doc.Char '\''] [Doc.Char '\'']]
  ]]

  [def-decl pub Doc.dquotes [-> [type N] [Doc N] [Doc N]]]
  [def-impl Doc.dquotes [\ [type N] self
    [self . Doc.surround [Doc.Char '"'] [Doc.Char '"']]
  ]]

  // Like Python's 'tuple' `(a, b, c)`
  [def-decl pub Doc.tupled [-> [type N] [Iter [Doc N]] [Doc N]]]
  [def-impl Doc.tupled [\ [type N] docs
    [docs . Doc.intercalate [Doc.String ", "] . Doc.parens]
  ]]

  // Like Python's (and most languages') list() `[a, b, c]`
  [def-decl pub Doc.listed [-> [type N] [Iter [Doc N]] [Doc N]]]
  [def-impl Doc.listed [\ [type N] docs
    [docs . Doc.intercalate [Doc.String ", "] . Doc.brackets]
  ]]

  // Like Python's 'set' `{a, b, c}`
  [def-decl pub Doc.setted [-> [type N] [Iter [Doc N]] [Doc N]]]
  [def-impl Doc.setted [\ [type N] docs
    [docs . Doc.intercalate [Doc.String ", "] . Doc.braces]
  ]]

  // Like the 'Pretty' typeclass of Haskell 'prettyprinter'
  [class-decl Doc.Pretty [A *]]
  [class-struct Doc.Pretty
    [field pretty [-> [type N] A [Doc N]]]
  ]

  [def-decl pub Doc.of [-> [type N] [type A] A [auto [Doc.Pretty A]] [Doc N]]]
  [def-impl Doc.of [\ [type N] [type A] self [auto A.Pretty] [A.Pretty . Doc.Pretty.pretty self]]]

  // Renders the 'Debug.repr' of a value as a 'Doc'.
  [def-decl pub Doc.of_repr [-> [type N] [type A] A [auto [Debug A]] [Doc N]]]
  [def-impl Doc.of_repr [\ [type N] [type A] self [auto A.Debug] [Doc.String [Debug.repr self]]]]

  [def-decl pub auto String.Doc.Pretty [Doc.Pretty String]]
  [def-impl String.Doc.Pretty [Doc.Pretty.new
    [\ [type N] a [Doc.String a]]
  ]]

  [def-decl pub auto Char.Doc.Pretty [Doc.Pretty Char]]
  [def-impl Char.Doc.Pretty [Doc.Pretty.new
    [\ [type N] a [Doc.Char a]]
  ]]

  [def-decl pub auto I32.Doc.Pretty [Doc.Pretty I32]]
  [def-impl I32.Doc.Pretty [Doc.Pretty.new
    [\ [type N] a [Doc.of [a . I32.to_str]]]
  ]]

  [def-decl pub auto Int.Doc.Pretty [Doc.Pretty Int]]
  [def-impl Int.Doc.Pretty [Doc.Pretty.new
    [\ [type N] a [Doc.of [a . Int.to_str]]]
  ]]

  [def-decl pub auto Bool.Doc.Pretty [Doc.Pretty Bool]]
  [def-impl Bool.Doc.Pretty [Doc.Pretty.new
    [\ [type N] a [Doc.String [if a "true" "false"]]]
  ]]
]
