[file
  [import "base.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/lexer.dsl"]
  [import "dsl/location.dsl"]

  [class-decl SyntaxError]
  [class-struct SyntaxError
    [field loc Loc]
    [field message String]
  ]

  // Parser state
  [class-decl P]
  [class-struct P
    // Inputs
    [field whole_src_loc Loc]
    [field tokens [Array Token]]
    [field cursor [Mut I32]]

    // Outputs
    [field out_syntax_errors [Array SyntaxError]]
  ]

  [def-decl P.add_syntax_error [-> P SyntaxError ,]]
  [def-impl P.add_syntax_error [\ self err [do
    [self . P.out_syntax_errors . Array.append err]
  ]]]

  [def-decl P.peek_token [-> P [Maybe Token]]]
  [def-impl P.peek_token [\ self [do
    [let cursor [self . P.cursor . get]]
    [let tokens [self . P.tokens]]
    [if [< cursor [tokens . Array.length]]
      [Some [tokens . Array.at cursor]]
      [None]
    ]
  ]]]

  [def-decl P.consume_token [-> P ,]]
  [def-impl P.consume_token [\ self [do
    [update [self . P.cursor] I32.suc]
  ]]]

  [def-decl P.next_token [-> P [Maybe Token]]]
  [def-impl P.next_token [\ self [do
    [let tok [self . P.peek_token]]
    [when [tok . Maybe.is_some] [\ _ [self . P.consume_token]]]
    tok
  ]]]

  [def-decl P.parse_expr [-> P [Maybe Expr]]]
  [def-decl P.parse_list [-> P Loc Loc Expr]]

  [def-impl P.parse_expr [\ self [do
    [match [self . P.next_token]
      [[None] [do
        // Case: Unexpected EOF
        [self . P.add_syntax_error [SyntaxError.new [self . P.whole_src_loc] "Unexpected end of input"]]
        [None]
      ]]
      [[Some [mk [; tokLoc tokData]]] [do
        [match tokData
          // Case: Begin a new list
          [[Open] [do
            [self . P.parse_list tokLoc tokLoc . Some]
          ]]

          // Case: Atom
          [[Atom atom] [do
            [Expr.new tokLoc [ExprData.Atom atom] . Some]
          ]]

          // Case: Unexpected token
          [_ [do
            [self . P.add_syntax_error [SyntaxError.new tokLoc "Unexpected token"]]
            [None]
          ]]
        ]
      ]]
    ]
  ]]]

  [def-impl P.parse_list [\ self openLoc last_known_loc [do
    [let items [Array.create ,]]
    [let* [go [\ last_known_loc [do
      [match [self . P.peek_token]
        [[None] [do
          // Case: Unexpected EOF while in a list
          [self . P.add_syntax_error [SyntaxError.new openLoc "Closing ']' is absent"]]
          [Expr.new [Loc.union openLoc last_known_loc] [ExprData.List items]]
        ]]
        [[Some [mk [; close_loc [Close]]]] [do
          // Case: List closes
          [self . P.consume_token]
          [Expr.new [Loc.union openLoc close_loc] [ExprData.List items]]
        ]]
        [[Some tok] [do
          // Case: List item
          [match [P.parse_expr self]
            [[Some item] [do
              // Got a valid list item
              [items . Array.append item]
              [go [item . Expr.loc]]
            ]]
            [[None] [do
              // Skip for error recovery
              [go last_known_loc]
            ]]
          ]
        ]]
      ]
    ]]]]
    [go last_known_loc]
  ]]]

  [def-decl P.mark_remaining_as_superfluous [-> P ,]]
  [def-impl P.mark_remaining_as_superfluous [\ self [do
    [let= [Some tok] [self . P.next_token]
      [[None] ,]]
    [self . P.add_syntax_error [SyntaxError.new [tok . Token.loc] "Superfluous token"]]
    [self . P.mark_remaining_as_superfluous]
  ]]]

  [def-decl P.parse_whole_file [-> P Expr]]
  [def-impl P.parse_whole_file [\ self [do
    [let expr [self . P.parse_expr]]
    [self . P.mark_remaining_as_superfluous]

    // If the entire file is so badly-written that we cannot even parse out an
    // expression, we use a '[]' as a substitute for error-tolerance
    [match expr
      [[None] [Expr.new [self . P.whole_src_loc] [ExprData.List [Array.create ,]]]]
      [[Some expr] expr]
    ]
  ]]]

  [class-decl DSL.parse_source.Result]
  [class-struct DSL.parse_source.Result
    [field expr Expr]
    [field syntax_errors [Array SyntaxError]]
  ]

  [def-decl pub DSL.parse_source [-> Dim String DSL.parse_source.Result]]
  [def-impl DSL.parse_source [\ dim input [do
    [let tokens [DSL.lex_source dim input]]
    [let syntax_errors [Array.create ,]]

    // First pass: Check for bad tokens and report them as syntax errors
    [tokens . Array.foreach [\ token [do
      [let= [mk [; loc [Bad content]]] token
        [_ continue]]
      [let err [SyntaxError.new loc [[vec "Bad token: '" content "'"] . Vec.iter . String.concat_iter]]]
      [syntax_errors . Array.append err]
      continue
    ]]]

    // Drop irrelevant tokens so the parser doesn't have to deal with them.
    [tokens . Array.filter_inplace [\ token
      [match [token . Token.data]
        [[Comment] false]
        [[Bad _] false]
        [[Spaces] false]
        [_ true]
      ]
    ]]

    // Parse through the whole thing and extract results
    [let parser [P.new* [dict
      [whole_src_loc [Loc.of_source dim input]]
      [tokens tokens]
      [out_syntax_errors syntax_errors]
      [cursor [var 0]]
    ]]]
    [let expr [parser . P.parse_whole_file]]
    [DSL.parse_source.Result.new* [dict
      [syntax_errors syntax_errors]
      [expr expr]
    ]]
  ]]]
]