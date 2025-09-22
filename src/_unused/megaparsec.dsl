[file
  [import "base.dsl"]

  [class-decl Megaparsec [A *]]
  [class-struct Megaparsec
    [field recognize [-> [type R] String I32 [-> I32 [Maybe A] R] R]]
  ]

  [def-decl pub Megaparsec.run* [-> [type A] [Megaparsec A] String I32 [; I32 [Maybe A]]]]
  [def-impl Megaparsec.run* [\ [type A] parser input cursor
    [parser . Megaparsec.recognize input cursor [\ cursor res [; cursor res]]]
  ]]

  [def-decl pub Megaparsec.run [-> [type A] [Megaparsec A] String [; I32 [Maybe A]]]]
  [def-impl Megaparsec.run [\ [type A] parser input
    [parser . Megaparsec.run* input 0]
  ]]

  [def-decl pub Megaparsec.accepts [-> [type A] [Megaparsec A] String Bool]]
  [def-impl Megaparsec.accepts [\ [type A] parser input [do
    [parser . Megaparsec.run input . Pair.snd . Maybe.isSome]
  ]]]

  [def-decl pub Megaparsec.then [-> [type A *] [type B *] [Megaparsec A] [-> A [Megaparsec B]] [Megaparsec B]]]
  [def-impl Megaparsec.then [\ [type A *] [type B *] m.a a->m.b
    [Megaparsec.new
      [\ [type R] input cursor next [do
        [Megaparsec.recognize m.a input cursor [\ cursor res
          [match res
            [[None] [next cursor [None]]]
            [[Some a] [Megaparsec.recognize [a->m.b a] input cursor next]]
          ]
        ]]
      ]]
    ]
  ]]

  [def-decl pub Megaparsec.pure [-> [type A *] A [Megaparsec A]]]
  [def-impl Megaparsec.pure
    [\ [type A *] a
      [Megaparsec.new [\ [type R] input cursor next [next cursor [Some a]]]]
    ]
  ]

  [def-decl pub Megaparsec.map [-> [type A *] [type B *] [Megaparsec A] [-> A B] [Megaparsec B]]]
  [def-impl Megaparsec.map [\ [type A *] [type B *] parser f
    [Megaparsec.then parser [\ a [Megaparsec.pure [f a]]]]
  ]]

  [def-decl pub Megaparsec.<*> [-> [type A] [type B] [Megaparsec [-> A B]] [Megaparsec A] [Megaparsec B]]]
  [def-impl Megaparsec.<*> [\ [type A] [type B] parserFunc parserArg
    [Megaparsec.then parserFunc [\ func
      [Megaparsec.then parserArg [\ arg
        [Megaparsec.pure [func arg]]
      ]]
    ]]
  ]]

  [def-decl pub auto Megaparsec.Functor [Functor Megaparsec]]
  [def-impl Megaparsec.Functor [Functor.new Megaparsec.map]]

  [def-decl pub auto Megaparsec.Pure [Pure Megaparsec]]
  [def-impl Megaparsec.Pure [Pure.new Megaparsec.pure]]

  [def-decl pub auto Megaparsec.Apply [Apply Megaparsec]]
  [def-impl Megaparsec.Apply [Apply.new Megaparsec.<*>]]

  [def-decl pub auto Megaparsec.Then [Then Megaparsec]]
  [def-impl Megaparsec.Then [Then.new Megaparsec.then]]

  [def-decl pub auto Megaparsec.AEmpty [AEmpty Megaparsec]]
  [def-impl Megaparsec.AEmpty [AEmpty.new
    [\ [type A *] [Megaparsec.new [\ [type R] _input cursor next [next cursor [None]]]]]
  ]]

  [def-decl pub auto Megaparsec.APlus [APlus Megaparsec]]
  [def-impl Megaparsec.APlus [APlus.new
    [\ [type A *] left right
      [Megaparsec.new
        [\ [type R] input cursor next
          [Megaparsec.recognize left input cursor [\ new_cursor res
            [match res
              [[Some a] [next new_cursor [Some a]]]
              [[None] [do
                [let has_consumed [new_cursor . > cursor]]
                [if has_consumed
                  [next new_cursor [None]]
                  [Megaparsec.recognize right input cursor next]
                ]
              ]]
            ]
          ]]
        ]
      ]
    ]
  ]]

  [def-decl pub Megaparsec.try [-> [type A] [Megaparsec A] [Megaparsec A]]]
  [def-impl Megaparsec.try [\ [type A] parser
    [Megaparsec.new
      [\ [type R] input cursor next
        [Megaparsec.recognize parser input cursor [\ new_cursor res
          [match res
            [[None] [next cursor [None]]] // NOTE: NOT **next new_cursor**. This is what 'try' is about
            [[Some a] [next new_cursor [Some a]]]
          ]
        ]]
      ]
    ]
  ]]

  [def-decl pub Megaparsec.lazy [-> [type A] [-> , [Megaparsec A]] [Megaparsec A]]]
  [def-impl Megaparsec.lazy [\ [type A] parserThunk
    [Megaparsec.new [\ [type R] input cursor next
      [Megaparsec.recognize [parserThunk ,] input cursor next]
    ]]
  ]]

  [def-decl pub Megaparsec.charWhere [-> [-> Char Bool] [Megaparsec Char]]]
  [def-impl Megaparsec.charWhere [\ predicate [Megaparsec.new
    [\ [type R] input cursor next [do
      [let view [ input . String.drop cursor ]]

      [let= [false] [view . String.isEmpty]
        [[true] [next cursor [None]]]]

      [let ch [view . String.at 0]]

      [if [predicate ch]
        [next [cursor . I32.+ 1] [Some ch]]
        [next cursor [None]]
      ]
    ]]
  ]]]

  [def-decl pub Megaparsec.anyChar [Megaparsec Char]]
  [def-impl Megaparsec.anyChar [Megaparsec.charWhere [Fn.const true]]]

  [def-decl pub Megaparsec.char [-> Char [Megaparsec Char]]]
  [def-impl Megaparsec.char [\ ch [Megaparsec.charWhere [Char.== ch]]]]

  [def-decl pub Megaparsec.string [-> String [Megaparsec ,]]]
  [def-impl Megaparsec.string [\ want [Megaparsec.new
    [\ [type R] input cursor next
      [if [input . String.drop cursor . String.startsWith want]
        [next [cursor . I32.+ [want . String.length]] [Some ,]]
        [next cursor [None]]
      ]
    ]
  ]]]

  [def-decl pub Megaparsec.takeWhile [-> [-> Char Bool] [Megaparsec String]]]
  [def-impl Megaparsec.takeWhile [\ predicate
    [Megaparsec.new [\ [type R] input cursor next [do
      [let view [input . String.drop cursor]]
      [let got [view . String.takeWhile predicate]]
      [let new_cursor [cursor . I32.+ [got . String.length]]]
      [next new_cursor [Some got]]
    ]]]
  ]]

  [def-decl pub Megaparsec.takeWhile1 [-> [-> Char Bool] [Megaparsec String]]]
  [def-impl Megaparsec.takeWhile1 [\ predicate [do
    [Megaparsec.takeWhile predicate . Megaparsec.then [\ str
      [if [str . String.isEmpty] [aempty] [Megaparsec.pure str]]]
    ]
  ]]]

  [def-decl pub Megaparsec.dropWhile [-> [-> Char Bool] [Megaparsec ,]]]
  [def-impl Megaparsec.dropWhile [\ predicate
    [Megaparsec.takeWhile predicate . void]
  ]]

  [def-decl pub Megaparsec.dropWhile1 [-> [-> Char Bool] [Megaparsec ,]]]
  [def-impl Megaparsec.dropWhile1 [\ predicate
    [Megaparsec.takeWhile1 predicate . void]
  ]]

  [def-decl pub Megaparsec.manyTill* [-> [type A] [type End] [Megaparsec A] [Megaparsec End] [Megaparsec [; End [Array A]]]]]
  [def-impl Megaparsec.manyTill* [\ [type A] [type End] pItem pEnd
    [Megaparsec.lazy [\ _ [do
      [let items [Array.create ,]]
      [let* [go
        [Alt.sum [vec
          [pEnd . map [\ end [; end items]]]
          [pItem . *> [Megaparsec.lazy [\ _ go]]]
        ]]
      ]]
      go
    ]]]
  ]]

  [def-decl pub Megaparsec.manyTill [-> [type A] [type End] [Megaparsec A] [Megaparsec End] [Megaparsec [Array A]]]]
  [def-impl Megaparsec.manyTill [\ [type A] [type End] pItem pEnd
    [Megaparsec.manyTill* pItem pEnd . Megaparsec.map [Pair.snd]]
  ]]

  // @lookAhead p@ parses @p@ without consuming any input.
  //
  // If @p@ fails and consumes some input, so does @lookAhead@. Combine with
  // 'try' if this is undesirable.
  [def-decl pub Megaparsec.lookAhead [-> [type A] [Megaparsec A] [Megaparsec A]]]
  [def-impl Megaparsec.lookAhead [\ [type A] parser
    [Megaparsec.new [\ [type R] input cursor next
      [Megaparsec.recognize parser input cursor [\ new_cursor res [do
        [let has_consumed [new_cursor . > cursor]]
        [match res
          [[None] [next new_cursor [None]]]
          [[Some a] [next cursor [Some a]]]
        ]
      ]]]
    ]]
  ]]

  [def-decl pub Megaparsec.peekChar [Megaparsec Char]]
  [def-impl Megaparsec.peekChar [Megaparsec.lookAhead Megaparsec.anyChar]]

  [def-decl pub Megaparsec.endOfInput [Megaparsec ,]]
  [def-impl Megaparsec.endOfInput
    [Megaparsec.new [\ [type R] input cursor next [do
      [let is_end [input . String.length . == cursor]]
      [if is_end
        [next cursor [Some ,]]
        [next cursor [None]]
      ]
    ]]]
  ]

  [def-decl pub Megaparsec.notFollowedBy [-> [type A] [Megaparsec A] [Megaparsec ,]]]
  [def-impl Megaparsec.notFollowedBy [\ [type A] parser
    [void [Megaparsec.try parser] . <|> [pure ,]]
  ]]

  [def-decl pub Megaparsec.escapableChar [Megaparsec Char]]
  [def-impl Megaparsec.escapableChar
    // FIXME: OPTIMIZE ME & READ Megaparsec's escapableChar
    [Alt.sum [vec
      [Megaparsec.string "\\n" . $> '\n']
      [Megaparsec.string "\\r" . $> '\r']
      [Megaparsec.string "\\t" . $> '\t']
      [Megaparsec.string "\\\\" . $> '\\']
      [Megaparsec.string "'" . $> '\'']
      [Megaparsec.string "\"" . $> '"']
      [Megaparsec.charWhere [\ ch [ch . != '\\']]]
    ]]
  ]

  [def-decl pub Megaparsec.charLiteral [Megaparsec Char]]
  [def-impl Megaparsec.charLiteral
    [Megaparsec.char '\''
      . *> Megaparsec.escapableChar
      . <* [Megaparsec.char '\'']
    ]
  ]

  [def-decl pub Megaparsec.stringLiteral [Megaparsec String]]
  [def-impl Megaparsec.stringLiteral
    [Megaparsec.char '"'
      . *> [Megaparsec.manyTill Megaparsec.escapableChar [Megaparsec.char '"']]
      . map String.fromArray
    ]
  ]

  // FIXME: Add and use BigInt
  [def-decl pub Megaparsec.decimal [Megaparsec Int]]
  [def-impl Megaparsec.decimal 
    [Megaparsec.takeWhile1 Char.isAsciiDigit
      . Megaparsec.map [\ str [do
        [let accum [var [Int.fromI32 0]]]
        [str . String.foreach [ \ ch [do
          [let digit [ch . Char.parseAsciiDigit . Maybe.unwrap . Int.fromI32]]
          [update accum [\ acc [acc . Int.* [Int.fromI32 10] . Int.+ digit]]]
          continue
        ]]]
        [get accum]
      ]]
    ]
  ]

  [def-decl pub Megaparsec.withNumericalSign [-> [Megaparsec Int] [Megaparsec Int]]]
  [def-impl Megaparsec.withNumericalSign [\ intMegaparsec [do
    [try modifier [Alt.sum [vec
      [Megaparsec.char '+' . $> [Fn.id]]
      [Megaparsec.char '-' . $> Int.negate]
      [Megaparsec.pure [Fn.id]]
    ]]]
    [intMegaparsec . map modifier]
  ]]]
]