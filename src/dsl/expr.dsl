[file
  [import "base.dsl"]
  [import "dsl/location.dsl"]
  [import "wlp.dsl"]

  [tydef Sym * String]

  [class-decl Atom]
  [class-enum Atom
    [member Sym Sym]
    [member Int Int]
    [member Char Char]
    [member Str String]
  ]

  [def-decl pub auto Atom.Debug [Debug Atom]]
  [def-impl Atom.Debug [Debug.new [\ self out [do
    [out . StringBuilder.append [match self
      [[Sym s] ["Sym(" . <> s . <> ")"]]
      [[Int i] ["Int(" . <> [Debug.repr i] . <> ")"]]
      [[Char c] ["Char(" . <> [Debug.repr c] . <> ")"]]
      [[Str s] ["Str(" . <> [Debug.repr s] . <> ")"]]
    ]]
  ]]]]

  [class-decl TokenData]
  [class-enum TokenData
    [member Spaces ,]
    [member Comment ,]
    [member Open ,]
    [member Close ,]
    [member Atom Atom]
    [member Bad String]
  ]

  [def-decl pub auto TokenData.Debug [Debug TokenData]]
  [def-impl TokenData.Debug [Debug.new [\ self out [do
    [out . StringBuilder.append [match self
      [[Spaces] "Spaces"]
      [[Comment] "Comment"]
      [[Open] "Open"]
      [[Close] "Close"]
      [[Atom atom] ["Atom(" . <> [Debug.repr atom] . <> ")"]]
      [[Bad s] ["Bad(" . <> s . <> ")"]]
    ]]
  ]]]]

  [class-decl Token]
  [class-struct Token
    [field loc Loc]
    [field data TokenData]
  ]

  [class-decl ExprData]
  [class-decl Expr]
  [class-enum ExprData
    [member List [Array Expr]]
    [member Atom Atom]
  ]
  [class-struct Expr
    [field loc Loc]
    [field data ExprData]
  ]

  // Like 'Expr', but does not have Loc info.
  [class-decl IExpr]
  [class-enum IExpr
    [member List [Array IExpr]]
    [member Atom Atom]
  ]

  [def-decl pub Expr.strip_loc [-> Expr IExpr]]
  [def-impl Expr.strip_loc [\ self [do
    [match [self . Expr.data]
      [[Atom atom] [IExpr.Atom atom]]
      [[List xs] [IExpr.List [xs . Array.map Expr.strip_loc]]]
    ]
  ]]]

  // Format as '[foo bar baz]'
  [def-decl pub pretty_slist [-> [type N] [Iter [Doc N]] [Doc N]]]
  [def-impl pretty_slist [\ [type N] docs
    [docs . Doc.hsep . Doc.brackets]
  ]]

  [def-decl pub Atom.pretty [-> [type N] Atom [Doc N]]]
  [def-impl Atom.pretty [\ [type N] self [do
    [match self
      [[Sym s] [Doc.of s]]
      [[Int i] [Doc.of i]]
      [[Char c] [Doc.of c . Doc.squotes]]
      [[Str s] [Doc.of s . Doc.dquotes]]
    ]
  ]]]

  [def-decl pub IExpr.pretty [-> [type N] IExpr [Doc N]]]
  [def-impl IExpr.pretty [\ [type N] self [do
    [match self
      [[Atom atom] [atom . Atom.pretty]]
      [[List xs] [xs . Array.iter . Iter.map [IExpr.pretty] . pretty_slist]]
    ]
  ]]]

  [def-decl pub Expr.pretty [-> [type N] Expr [Doc N]]]
  [def-impl Expr.pretty [\ [type N] self [do
    [self . Expr.strip_loc . IExpr.pretty]
  ]]]

  [def-decl pub auto Atom.Doc.Pretty [Doc.Pretty Atom]]
  [def-impl Atom.Doc.Pretty [Doc.Pretty.new Atom.pretty]]

  [def-decl pub auto IExpr.Doc.Pretty [Doc.Pretty IExpr]]
  [def-impl IExpr.Doc.Pretty [Doc.Pretty.new IExpr.pretty]]

  [def-decl pub auto Expr.Doc.Pretty [Doc.Pretty Expr]]
  [def-impl Expr.Doc.Pretty [Doc.Pretty.new Expr.pretty]]
]