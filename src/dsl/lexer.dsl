[file
  [import "base.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/location.dsl"]
  [import "zeptoparsec.dsl"]

  [def-decl p_spaces_1 [Zeptoparsec ,]]
  [def-impl p_spaces_1 [Zeptoparsec.drop_while1 Char.is_space]]

  [def-decl special_chars_set I32Set]
  [def-impl special_chars_set [do
    [let chvec [vec '!' '#' '$' '%' '&' '*' '+' '.' '/' '<' '=' '>' '?' '@' '\\' '^' '|' '-' '~' ',' '_' ',' ':' ';']]
    [let set [I32MutSet.create ,]]
    [chvec . Vec.foreach [\ ch [do
      [set . I32MutSet.add [ch . Char.ord]]
      continue
    ]]]
    [set . I32MutSet.freeze]
  ]]

  [def-decl is_special_char [-> Char Bool]]
  [def-impl is_special_char [\ ch [special_chars_set . I32Set.has [ch . Char.ord]]]]

  [def-decl is_head [-> Char Bool]]
  [def-impl is_head [\ ch [do
    [let= [false] [Char.is_alpha ch]
      [[true] true]]
    [is_special_char ch]
  ]]]

  [def-decl is_tail [-> Char Bool]]
  [def-impl is_tail [\ ch [do
    [let= [false] [Char.is_alpha_num ch]
      [[true] true]]
    [let= [false] [ch . Char.== '\'']
      [[true] true]]
    [is_special_char ch]
  ]]]

  [def-decl p_sym [Zeptoparsec Sym]]
  [def-impl p_sym [do
    [try head [Zeptoparsec.char_where is_head]]
    [try tail [Zeptoparsec.take_while is_tail]]
    [let sym [String.cons head tail]]
    [Zeptoparsec.pure sym]
  ]]

  [def-decl p_line_comment [Zeptoparsec ,]]
  [def-impl p_line_comment [do
    [try _ [Zeptoparsec.string "//"]]
    [Zeptoparsec.drop_while [Char.!= '\n']]
  ]]

  [def-decl p_block_comment [Zeptoparsec ,]]
  [def-impl p_block_comment [do
    [try _ [Zeptoparsec.string "/*"]]

    // NOTE: It is possible the user forgets to close the block comment, in
    // which case we will just consume the rest of the file as a comment and
    // report no error.
    [try _ [Zeptoparsec.many_before [Zeptoparsec.any_char] [Zeptoparsec.string "*/"]]]
    [try _ [Alt.optional [Zeptoparsec.string "*/"]]]
    [pure ,]
  ]]

  [def-decl is_not_bad [-> Char Bool]]
  [def-impl is_not_bad [\ ch [do
    [let= [false] [ch . Char.== '[']
      [[true] true]]
    [let= [false] [ch . Char.== ']']
      [[true] true]]
    [Char.is_space ch]
  ]]]

  [def-decl p_bad [Zeptoparsec String]]
  [def-impl p_bad [do
    [try head Zeptoparsec.any_char]
    [try tail [Zeptoparsec.take_while is_not_bad]]
    [let bad [String.cons head tail]]
    [Zeptoparsec.pure bad]
  ]]

  [def-decl p_token_data [Zeptoparsec TokenData]]
  [def-impl p_token_data [Foldable.asum [vec
    [p_spaces_1 . $> TokenData.Spaces]
    [Zeptoparsec.char '[' . $> TokenData.Open]
    [Zeptoparsec.char ']' . $> TokenData.Close]
    [p_line_comment . $> TokenData.Comment]
    [p_block_comment . $> TokenData.Comment]
    [Zeptoparsec.string_literal
      . map [Atom.Str . fn.| TokenData.Atom]]
    [Zeptoparsec.char_literal
      . map [Atom.Char . fn.| TokenData.Atom]]
    [Zeptoparsec.with_numerical_sign Zeptoparsec.decimal
      . map [Atom.Int . fn.| TokenData.Atom]]
    [p_sym . map [Atom.Sym . fn.| TokenData.Atom]]
    [p_bad . map TokenData.Bad]
  ]]]

  [def-decl pub DSL.lex_source [-> Dim String [Array Token]]]
  [def-impl DSL.lex_source [\ dim input [do
    [let cursor [var 0]]
    [let start_pos [var [Pos.new 0 0]]]
    [let items [Array.create ,]]

    [loop [\ _ [do
      [let= [false] [[get cursor] . >= [input . String.length]]
        [_ break]]

      [let= [; next_cursor tok_data] [p_token_data
        . Zeptoparsec.continue input [get cursor]
        . Maybe.expect "lex_source: p_token_data should never fail"
      ]]

      [let stop_pos [[get start_pos] . Pos.advance_by_string [input . String.slice [get cursor] next_cursor]]]
      [let span [Span.new [get start_pos] stop_pos]]
      [let loc [Loc.new dim span]]
      [items . Array.append [Token.new loc tok_data]]

      [set start_pos stop_pos]
      [set cursor next_cursor]
      continue
    ]]]

    items
  ]]]
]