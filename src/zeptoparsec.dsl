[file
  [import "base.dsl"]

  [class-decl Zeptoparsec [A *]]
  [class-struct Zeptoparsec
    // NOTE: This signature allows `Zeptoparsec.then` to be tail recursive.
    [field continue [-> String I32 [Maybe [; I32 A]]]]
  ]

  [def-decl pub Zeptoparsec.run [-> [type A *] [Zeptoparsec A] String [Maybe [; I32 A]]]]
  [def-impl Zeptoparsec.run [\ [type A *] parser input
    [parser . Zeptoparsec.continue input 0]
  ]]

  [def-decl pub Zeptoparsec.accepts [-> [type A *] [Zeptoparsec A] String Bool]]
  [def-impl Zeptoparsec.accepts [\ [type A *] parser input [do
    [parser . Zeptoparsec.run input . Maybe.is_some]
  ]]]

  [def-decl pub Zeptoparsec.then [-> [type A *] [type B *] [Zeptoparsec A] [-> A [Zeptoparsec B]] [Zeptoparsec B]]]
  [def-impl Zeptoparsec.then [\ [type A *] [type B *] first next
    [Zeptoparsec.new
      [\ input cursor [do
        [match [first . Zeptoparsec.continue input cursor]
          [[Some [; cursor a]]
            [next a . Zeptoparsec.continue input cursor]]
          [[None]
            [None]]
        ]
      ]]
    ]
  ]]

  [def-decl pub Zeptoparsec.pure [-> [type A *] A [Zeptoparsec A]]]
  [def-impl Zeptoparsec.pure
    [\ [type A *] value [Zeptoparsec.new [\ _ cursor [Some [; cursor value]]]]]]

  [def-decl pub Zeptoparsec.map [-> [type A *] [type B *] [Zeptoparsec A] [-> A B] [Zeptoparsec B]]]
  [def-impl Zeptoparsec.map [\ [type A *] [type B *] parser f
    [Zeptoparsec.then parser [\ a [Zeptoparsec.pure [f a]]]]
  ]]

  [def-decl pub Zeptoparsec.<*> [-> [type A] [type B] [Zeptoparsec [-> A B]] [Zeptoparsec A] [Zeptoparsec B]]]
  [def-impl Zeptoparsec.<*> [\ [type A] [type B] parserFunc parserArg [do
    [Zeptoparsec.then parserFunc [\ func
      [Zeptoparsec.then parserArg [\ arg
        [Zeptoparsec.pure [func arg]]
      ]]
    ]]
  ]]]

  [def-decl pub auto Zeptoparsec.Functor [Functor Zeptoparsec]]
  [def-impl Zeptoparsec.Functor [Functor.new Zeptoparsec.map]]

  [def-decl pub auto Zeptoparsec.Pure [Pure Zeptoparsec]]
  [def-impl Zeptoparsec.Pure [Pure.new Zeptoparsec.pure]]

  [def-decl pub auto Zeptoparsec.Apply [Apply Zeptoparsec]]
  [def-impl Zeptoparsec.Apply [Apply.new Zeptoparsec.<*>]]

  [def-decl pub auto Zeptoparsec.Then [Then Zeptoparsec]]
  [def-impl Zeptoparsec.Then [Then.new Zeptoparsec.then]]

  [def-decl pub auto Zeptoparsec.AEmpty [AEmpty Zeptoparsec]]
  [def-impl Zeptoparsec.AEmpty [AEmpty.new
    [\ [type A *] [Zeptoparsec.new [\ _ _ [None]]]]
  ]]

  [def-decl pub auto Zeptoparsec.APlus [APlus Zeptoparsec]]
  [def-impl Zeptoparsec.APlus [APlus.new
    [\ [type A *] left right
      [Zeptoparsec.new
        [\ input cursor [do
          [match [left . Zeptoparsec.continue input cursor]
            [[Some result] [Some result]]
            [None [right . Zeptoparsec.continue input cursor]]
          ]
        ]]
      ]
    ]
  ]]

  [def-decl pub Zeptoparsec.char_where [-> [-> Char Bool] [Zeptoparsec Char]]]
  [def-impl Zeptoparsec.char_where [\ predicate [Zeptoparsec.new
    [\ input cursor [do
      [let view [ input . String.drop cursor ]]

      [let= [false] [view . String.is_empty]
        [[true] [None]]]

      [let ch [view . String.at 0]]

      [if [predicate ch]
        [Some [; [ cursor . I32.+ 1 ] ch]]
        [None]
      ]
    ]]
  ]]]

  [def-decl pub Zeptoparsec.any_char [Zeptoparsec Char]]
  [def-impl Zeptoparsec.any_char [Zeptoparsec.char_where [fn.const true]]]

  [def-decl pub Zeptoparsec.char [-> Char [Zeptoparsec Char]]]
  [def-impl Zeptoparsec.char [\ ch [Zeptoparsec.char_where [Char.== ch]]]]

  [def-decl pub Zeptoparsec.string [-> String [Zeptoparsec ,]]]
  [def-impl Zeptoparsec.string [\ want [Zeptoparsec.new
    [\ input cursor
      [if [input . String.drop cursor . String.starts_with want]
        [Some [; [cursor . I32.+ [want . String.length]] ,]]
        [None]
      ]
    ]
  ]]]

  [def-decl pub Zeptoparsec.many_before [-> [type A *] [type End *] [Zeptoparsec A] [Zeptoparsec End] [Zeptoparsec [Array A]]]]
  [def-impl Zeptoparsec.many_before [\ [type A *] [type End *] pItem pEnd
    [Zeptoparsec.new
      [\ input cursor [do
        [let items [Array.create ,]]
        [let cursor [var cursor]]
        [loop [\ _ [do
          [let= [false] [pEnd . Zeptoparsec.accepts [input . String.drop [get cursor]]]
            [[true] break]]
          [let= [Some [; cursor_ item]] [pItem . Zeptoparsec.continue input [get cursor]]
            [_ break]]
          [set cursor cursor_]
          [items . Array.append item]
          continue
        ]]]
        [Some [; [get cursor] items]]
      ]]
    ]
  ]]

  [def-decl pub Zeptoparsec.many_till [-> [type A *] [type End *] [Zeptoparsec A] [Zeptoparsec End] [Zeptoparsec [Array A]]]]
  [def-impl Zeptoparsec.many_till [\ [type A *] [type End *] pItem pEnd
    [Zeptoparsec.many_before pItem pEnd . <* pEnd]
  ]]

  [def-decl pub Zeptoparsec.take_while [-> [-> Char Bool] [Zeptoparsec String]]]
  [def-impl Zeptoparsec.take_while [\ predicate
    [Zeptoparsec.new
      [\ input cursor [do
        [let view [ input . String.drop cursor ]]
        [let got [view . String.take_while predicate]]
        [let cursor_ [cursor . I32.+ [got . String.length]]]
        [Some [; cursor_ got]]
      ]]
    ]
  ]]

  [def-decl pub Zeptoparsec.take_while1 [-> [-> Char Bool] [Zeptoparsec String]]]
  [def-impl Zeptoparsec.take_while1 [\ predicate [do
    [Zeptoparsec.take_while predicate . Zeptoparsec.then [\ str
      [if [str . String.is_empty] [aempty] [Zeptoparsec.pure str]]]
    ]
  ]]]

  [def-decl pub Zeptoparsec.drop_while [-> [-> Char Bool] [Zeptoparsec ,]]]
  [def-impl Zeptoparsec.drop_while [\ predicate
    [Zeptoparsec.take_while predicate . void]
  ]]

  [def-decl pub Zeptoparsec.drop_while1 [-> [-> Char Bool] [Zeptoparsec ,]]]
  [def-impl Zeptoparsec.drop_while1 [\ predicate
    [Zeptoparsec.take_while1 predicate . void]
  ]]

  [def-decl pub Zeptoparsec.look_ahead [-> [type A *] [Zeptoparsec A] [Zeptoparsec A]]]
  [def-impl Zeptoparsec.look_ahead [\ [type A *] parser
    [Zeptoparsec.new
      [\ input cursor [do
        [match [parser . Zeptoparsec.continue input cursor]
          [[Some [; _ a]] [Some [; cursor a]]]
          [None [None]]
        ]
      ]]
    ]
  ]]

  [def-decl pub Zeptoparsec.peek_char [Zeptoparsec Char]]
  [def-impl Zeptoparsec.peek_char [Zeptoparsec.look_ahead Zeptoparsec.any_char]]

  [def-decl pub Zeptoparsec.skip_n [-> I32 [Zeptoparsec ,]]]
  [def-impl Zeptoparsec.skip_n [\ n
    [Zeptoparsec.new
      [\ input cursor [do
        [let cursor [cursor . I32.+ n]]
        [let cursor [I32.min cursor [input . String.length]]]
        [Some [; cursor ,]]
      ]]
    ]
  ]]

  [def-decl pub Zeptoparsec.end_of_input [Zeptoparsec ,]]
  [def-impl Zeptoparsec.end_of_input
    [Zeptoparsec.new
      [\ input cursor [do
        [let end? [input . String.length . == cursor]]
        [if end?
          [Some [; cursor ,]]
          [None]
        ]
      ]]
    ]
  ]

  [def-decl escapable_char.table [StringMap Char]]
  [def-impl escapable_char.table [do
    [let map [StringMutMap.create ,]]
    [StringMutMap.set map "\\n" '\n']
    [StringMutMap.set map "\\r" '\r']
    [StringMutMap.set map "\\t" '\t']
    [StringMutMap.set map "\\b" '\b']
    [StringMutMap.set map "\\f" '\f']
    [StringMutMap.set map "\\\\" '\\']
    [StringMutMap.set map "\\\"" '"']
    [StringMutMap.set map "\\'" '\'']
    [StringMutMap.freeze map]
  ]]

  [def-decl pub Zeptoparsec.escapable_char [Zeptoparsec Char]]
  [def-impl Zeptoparsec.escapable_char
    // NOTE: See Megaparsec's escapable_char
    //
    // TODO: This function should be optimized to the MAX
    [Zeptoparsec.new [\ input cursor [do
      [let view [input . String.drop cursor]]

      [let first_2_chars [view . String.take 2]]
      [if-let [Some value] [escapable_char.table . StringMap.get? first_2_chars]
        [Some [; [cursor . I32.+ 2] value]]
      ]

      [Zeptoparsec.char_where [Char.!= '\\'] . Zeptoparsec.continue input cursor]
    ]]]
  ]

  [def-decl pub Zeptoparsec.char_literal [Zeptoparsec Char]]
  [def-impl Zeptoparsec.char_literal
    [Zeptoparsec.char '\''
      . *> Zeptoparsec.escapable_char
      . <* [Zeptoparsec.char '\'']
    ]
  ]

  [def-decl pub Zeptoparsec.string_literal [Zeptoparsec String]]
  [def-impl Zeptoparsec.string_literal
    [Zeptoparsec.char '"'
      . *> [Zeptoparsec.many_till Zeptoparsec.escapable_char [Zeptoparsec.char '"']]
      . map String.from_array
    ]
  ]

  // FIXME: Add and use BigInt
  [def-decl pub Zeptoparsec.decimal [Zeptoparsec Int]]
  [def-impl Zeptoparsec.decimal 
    [Zeptoparsec.take_while1 Char.is_ascii_digit
      . Zeptoparsec.map [\ str [do
        [let accum [var [Int.from_i32 0]]]
        [str . String.foreach [ \ ch [do
          [let digit [ch . Char.parse_ascii_digit . Maybe.expect "'ch' should be an ASCII digit" . Int.from_i32]]
          [update accum [\ acc [acc . Int.* [Int.from_i32 10] . Int.+ digit]]]
          continue
        ]]]
        [get accum]
      ]]
    ]
  ]

  [def-decl pub Zeptoparsec.with_numerical_sign [-> [Zeptoparsec Int] [Zeptoparsec Int]]]
  [def-impl Zeptoparsec.with_numerical_sign [\ intZeptoparsec [do
    [try modifier [Foldable.asum [vec
      [Zeptoparsec.char '+' . $> [fn.id]]
      [Zeptoparsec.char '-' . $> Int.negate]
      [Zeptoparsec.pure [fn.id]]
    ]]]
    [intZeptoparsec . map modifier]
  ]]]
]