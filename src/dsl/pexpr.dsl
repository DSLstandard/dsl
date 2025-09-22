[file
  [import "base.dsl"]
  [import "wlp.dsl"]
  [import "dsl/expr.dsl"]

  [class-decl PErrorEntry]
  [class-struct PErrorEntry
    [field expr Expr]
    [field desc [Doc Void]]
  ]

  [class-decl PError]
  [class-struct PError
    [field entries [DList PErrorEntry]]
  ]

  [def-decl PError.one [-> Expr [Doc Void] PError]]
  [def-impl PError.one [\ expr msg [
    [PError.new [DList.one [PErrorEntry.new expr msg]]]
  ]]]

  [def-decl PError.<> [-> PError PError PError]]
  [def-impl PError.<> [\ a b [
    [PError.new [[a . PError.entries] . <> [b . PError.entries]]]
  ]]]

  [def-decl pub auto PError.Semigroup [Semigroup PError]]
  [def-impl PError.Semigroup [Semigroup.new [PError.<>]]]

  [def-decl PError.empty PError]
  [def-impl PError.empty [PError.new [DList.empty]]]

  [def-decl pub PError.to_array [-> PError [Array PErrorEntry]]]
  [def-impl PError.to_array [\ self [self . PError.entries . DList.to_array]]]

  [tydef Consumed * Bool]

  [class-decl PExpr [A *]]
  [class-struct PExpr
    [field formatted [DList [Doc Void]]]
    [field recognize [-> [type R]
      Expr /* Expr to recognize */
      PError /* Propagated errors from previous patterns */
      Consumed /* Propagated "has consumed input" from previous patterns */
      [-> Consumed [Either PError A] R] /* Continuation */
      R
    ]]
  ]

  [class-decl PList [A *]]
  [class-struct PList
    [field formatted [DList [Doc Void]]]
    [field recognize [-> [type R]
      Expr /* Ref to the list expression */
      [Array Expr] /* The list items */
      PError /* Propagated errors from previous patterns */
      I32 /* Current index in the list to look at */
      [-> I32 [Either PError A] R] /* Continuation */
      R
    ]]
  ]

  [def-decl pub auto PExpr.Functor [Functor PExpr]]
  [def-impl PExpr.Functor [Functor.new [\ [type A] [type B] self a->b
    [PExpr.new
      [self . PExpr.formatted]
      [\ [type R] expr err consumed next [
        self . PExpr.recognize expr err consumed [\ consumed1 res [
          next consumed1 [res . map a->b]
        ]]
      ]
    ]]
  ]]]

  [def-decl pub auto PExpr.or_else [-> [type A] [PExpr A] [PExpr A] [PExpr A]]]
  [def-impl PExpr.or_else [\ [type A] p1 p2 [PExpr.new
    [[p1 . PExpr.formatted] . DList.<> [p2 . PExpr.formatted]]
    [\ [type R] expr err consumed next [
      [p1 . PExpr.recognize expr PError.empty false [\ a_consumed a_result [match a_result
        [[R a] [next a_consumed [Right a]]]
        [[L a_err]
          [if a_consumed
            [next true [Left a_err]]
            [p2 . PExpr.recognize expr [err . PError.<> a_err] consumed next]
          ]
        ]
      ]]]
    ]]
  ]]]

  [def-decl pub auto PExpr.neutral [-> [type A] [PExpr A]]]
  [def-impl PExpr.neutral [\ [type A] [PExpr.new
    [DList.empty]
    [\ [type R] _expr err consumed next [
      [next consumed [Left err]]
    ]]
  ]]]

  [def-decl pub auto PExpr.Semigroup [-> [type A] [Semigroup [PExpr A]]]]
  [def-impl PExpr.Semigroup [\ [type A] [Semigroup.new [PExpr.or_else]]]]

  [def-decl pub auto PExpr.Neutral [-> [type A] [Neutral [PExpr A]]]]
  [def-impl PExpr.Neutral [\ [type A] [Neutral.new [PExpr.neutral]]]]

  [def-decl pub auto PList.Functor [Functor PList]]
  [def-impl PList.Functor [Functor.new [\ [type A] [type B] self a->b
    [PList.new
      [self . PList.formatted]
      [\ [type R] expr items err idx next [
        self . PList.recognize expr items err idx [\ consumed res [
          next consumed [res . map a->b]
        ]]
      ]
    ]]
  ]]]

  [def-decl pub auto PList.Apply [Apply PList]]
  [def-impl PList.Apply [Apply.new [\ [type A] [type B] f.a->b f.a
    [PList.new
      [[f.a->b . PList.formatted] . DList.<> [f.a . PList.formatted]]
      [\ [type R] expr items err i next
        [PList.recognize f.a->b expr items err i [\ i res1 [match res1
          [[L err]
            [next i [Left err]]
          ]
          [[R a->b]
            [PList.recognize f.a expr items err i [\ i res2
              [next i [map res2 a->b]]
            ]]
          ]
        ]]]
      ]
    ]
  ]]]

  [def-decl pub auto PList.Pure [Pure PList]]
  [def-impl PList.Pure [Pure.new [\ [type A] a
    [PList.new
      [DList.empty]
      [\ [type R] expr items err i next [
        [next i [Right a]]
      ]
    ]]
  ]]]

  [def-decl pub auto PList.APlus [APlus PList]]
  [def-impl PList.APlus [APlus.new [\ [type A] xs ys
    [PList.new
      [[xs . PList.formatted] . DList.<> [ys . PList.formatted]]
      [\ [type R] expr items err i next
        [xs . PList.recognize expr items err i [\ new_i res [
          [match res
            [[R a] [next new_i [Right a]]]
            [[L err] [ys . PList.recognize expr items err i next]]
          ]
        ]]]
      ]
    ]
  ]]]

  [def-decl pub auto PList.AEmpty [AEmpty PList]]
  [def-impl PList.AEmpty [AEmpty.new [\ [type A]
    [PList.new
      [DList.empty]
      [\ [type R] expr items err i next
        [next i [Left err]]
      ]
    ]
  ]]]

  [def-decl pub PList.try [-> [type A] [PList A] [PList A]]]
  [def-impl PList.try [\ [type A] self
    [PList.new
      [self . PList.formatted]
      [\ [type R] expr items err i next
        [self . PList.recognize expr items err i [\ i_new res
          [match res
            [[R a] [next i_new [Right a]]]
            [[L err] [next i [Left err]]]
          ]
        ]]
      ]
    ]
  ]]

  [def-decl pretty_alternatives [-> [type N] [Iter [Doc N]] [Doc N]]]
  [def-impl pretty_alternatives [\ [type N] docs [do
    // TODO: This is not optimized but it is ok for small inputs
    [let docs [docs . List.from_iter]]
    [if [docs . List.is_one]
      [docs . List.head]
      [docs
        . List.intersperse [Doc.of "|"]
        . List.iter
        . Doc.hsep
        . Doc.braces
      ]
    ]
  ]]]

  [def-decl pub PExpr.label* [-> [type A] [Doc Void] [PExpr A] [PExpr A]]]
  [def-impl PExpr.label* [\ [type A] label self
    [self . PExpr.=formatted [DList.one label]]
  ]]

  [def-decl pub PExpr.label [-> [type A] String [PExpr A] [PExpr A]]]
  [def-impl PExpr.label [\ [type A] label [PExpr.label* [Doc.of label]]]]

  [def-decl pub PList.label* [-> [type A] [Doc Void] [PList A] [PList A]]]
  [def-impl PList.label* [\ [type A] label self
    [self . PList.=formatted [DList.one label]]
  ]]

  [def-decl pub PList.label [-> [type A] String [PList A] [PList A]]]
  [def-impl PList.label [\ [type A] label [PList.label* [Doc.of label]]]]

  [def-decl pub PExpr.format [-> [type A] [PExpr A] [Doc Void]]]
  [def-impl PExpr.format [\ [type A] self
    [self . PExpr.formatted . DList.to_array . Array.iter . pretty_alternatives]
  ]]

  [def-decl pub PList.format [-> [type A] [PList A] [Doc Void]]]
  [def-impl PList.format [\ [type A] self
    [self . PList.formatted . DList.to_array . Array.iter . pretty_slist]
  ]]

  [def-decl pub PExpr.match [-> [type A] [PExpr A] Expr [Either PError A]]]
  [def-impl PExpr.match [\ [type A] self expr
    [self . PExpr.recognize expr PError.empty false [\ _consumed res res]]
  ]]

  [def-decl pub PList.match [-> [type A] [PList A] Expr [Array Expr] [Either PError A]]]
  [def-impl PList.match [\ [type A] self expr items [do
    [let add_bad_format [\ [err PError]
      [err . <> [PError.one expr [Doc.of "Cannot match expression format: " . <> [self . PList.format]]]]
    ]]
    [self . PList.recognize expr items PError.empty 0 [\ i res
      [match res
        [[L err] [Left [add_bad_format err]]]
        [[R a]
          [if [i . == [items . Array.length]]
            [Right a]
            [Left [add_bad_format PError.empty]]
          ]
        ]
      ]
    ]]
  ]]]

  [def-decl mk_test_expr [-> [type A] [Doc Void] [-> Expr [Maybe A]] [PExpr A]]]
  [def-impl mk_test_expr [\ [type A] formatted recognize
    [PExpr.new
      [DList.one formatted]
      [\ [type R] expr err consumed next
        [match [recognize expr]
          [[Some a] [next true [Right a]]]
          [[None] [next consumed
            [Left [err . <> [PError.one expr [Doc.of "Cannot match expression format: " . <> formatted]]]]
          ]]
        ]
      ]
    ]
  ]]

  [def-decl pub PExpr.any_sym [PExpr Sym]]
  [def-impl PExpr.any_sym [mk_test_expr [Doc.of "<sym>"] [\ expr
    [match expr
      [[mk [; _ [Atom [Sym sym]]]] [Some sym]]
      [_ [None]]
    ]
  ]]]

  [def-decl pub PExpr.sym [-> Sym [PExpr ,]]]
  [def-impl PExpr.sym [\ expected [mk_test_expr [Doc.of expected] [\ expr [do
    [match expr
      [[mk [; _ [Atom [Sym sym]]]] [where [sym . == expected]] [Some ,]]
      [_ [None]]
    ]
  ]]]]]

  [def-decl pub PExpr.any_str [PExpr String]]
  [def-impl PExpr.any_str [mk_test_expr [Doc.of "<str>"] [\ expr
    [match expr
      [[mk [; _ [Atom [Str str]]]] [Some str]]
      [_ [None]]
    ]
  ]]]

  [def-decl pub PExpr.any_int [PExpr Int]]
  [def-impl PExpr.any_int [mk_test_expr [Doc.of "<int>"] [\ expr
    [match expr
      [[mk [; _ [Atom [Int int]]]] [Some int]]
      [_ [None]]
    ]
  ]]]

  [def-decl pub PExpr.any_char [PExpr Char]]
  [def-impl PExpr.any_char [mk_test_expr [Doc.of "<char>"] [\ expr
    [match expr
      [[mk [; _ [Atom [Char ch]]]] [Some ch]]
      [_ [None]]
    ]
  ]]]

  [def-decl pub PExpr.any_expr [PExpr Expr]]
  [def-impl PExpr.any_expr [mk_test_expr [Doc.of "<expr>"] [Some]]]

  [def-decl pub PExpr.captures [-> [type A] [PExpr [-> Expr A]] [PExpr A]]]
  [def-impl PExpr.captures [\ [type A] pexpr [PExpr.new
    [pexpr . PExpr.formatted]
    [\ [type R] expr err consumed next
      [PExpr.recognize pexpr expr err consumed [\ consumed res
        [next consumed [res . map [\ expr->a [expr->a expr]]]]
      ]]
    ]
  ]]]

  [def-decl pub PExpr.capture [-> [type A] [PExpr A] [PExpr [; Expr A]]]]
  [def-impl PExpr.capture [\ [type A] pexpr
    [pexpr . map [\ a expr [; expr a]] . PExpr.captures]
  ]]

  [def-decl pub PExpr.capture_ [-> [type A] [PExpr A] [PExpr Expr]]]
  [def-impl PExpr.capture_ [\ [type A] pexpr
    [pexpr . map [\ _ expr [expr]] . PExpr.captures]
  ]]

  [def-decl pub PExpr.list [-> [type A] [PList A] [PExpr A]]]
  [def-impl PExpr.list [\ [type A] pl
    [PExpr.new
      [DList.one [pl . PList.format]]
      [\ [type R] expr err in_consumed next [do
        [let mk_bad_error [\ _
          [PError.one expr [Doc.of "Cannot match expression format: " . <> [pl . PList.format]]]]]

        [else-let [mk [; _ [List items]]] expr [do
          [next in_consumed [Left [err . <> [mk_bad_error ,]]]]
        ]]

        [PList.recognize pl expr items err 0 [\ i res [do
          [let consumed [in_consumed . Bool.or [i . > 0]]]
          [match res
            [[L err] [next consumed [Left err]]]
            [[R a]
              [if [i . == [items . Array.length]]
                [next consumed [Right a]]
                [next consumed [Left [mk_bad_error ,]]]
              ]
            ]
          ]
        ]]]
      ]]
    ]
  ]]

  [def-decl pub PList.item [-> [type A] [PExpr A] [PList A]]]
  [def-impl PList.item [\ [type A] pexpr
    [PList.new
      [DList.one [PExpr.format pexpr]]
      [\ [type R] self items err i next
        [match [Array.at? items i]
          [[None]
            [next i [Left [err . <> [PError.one self [Doc.of "Missing list expression item: " . <> [PExpr.format pexpr]]]]]]
          ]
          [[Some expr]
            [PExpr.recognize pexpr expr err false [\ consumed
              [next [if consumed [i . I32.suc] i]]]]
          ]
        ]
      ]
    ]
  ]]

  [class-decl PExpr.Block]
  [class-struct PExpr.Block
    [field self Expr]
    [field leader Expr]
  ]

  [def-decl pub PExpr.block [-> [type A] Sym [PList [-> PExpr.Block A]] [PExpr A]]]
  [def-impl PExpr.block [\ [type A] leader args
    [PExpr.captures [PExpr.list [
      [\ leader f self [f [PExpr.Block.new self leader]]]
      . <$> [PList.try [PList.item [PExpr.capture_ [PExpr.sym leader]]]]
      . <*> args
    ]]]
  ]]

  [def-decl pub PExpr.block_ [-> [type A] Sym [PList A] [PExpr A]]]
  [def-impl PExpr.block_ [\ [type A] leader args
    [PExpr.block leader [args . map [\ a _block a]]]
  ]]

  [def-decl pub PList.many [-> [type A] [PList A] [PList [Array A]]]]
  [def-impl PList.many [\ [type A] pItem [PList.new
    [DList.one [<> [PList.format pItem] [Doc.of "*"]]]
    [\ [type R] self items err in_i next [do
      [let* [go [\ [i I32] [accum [DList A]]
        [pItem . PList.recognize self items err i [\ i_new res
          [match res
            [[L _err] [next i [Right [DList.to_array accum]]]]
            [[R a]
              [go i_new [accum . DList.snoc a]]
            ]
          ]
        ]]
      ]]]
      [go in_i [DList.empty]]
    ]]
  ]]]

  [def-decl pub PList.some [-> [type A] [PList A] [PList [Array A]]]]
  [def-impl PList.some [\ [type A] pItem
    [PList.label* [PList.format pItem . <> [Doc.of "+"]] [
      [\ x xs [do
        [xs . Array.prepend x]
        xs
      ]]
      . <$> pItem
      . <*> [PList.many pItem]
    ]]
  ]]

  [def-decl pub PList.optional [-> [type A] [PList A] [PList [Maybe A]]]]
  [def-impl PList.optional [\ [type A] pItem
    [PList.label* [PList.format pItem . <> [Doc.of "?"]]
      [Alt.optional pItem]
    ]
  ]]

  [def-decl pub PList.one_of [-> [type A] [PExpr A] [PList A]]]
  [def-impl PList.one_of [\ [type A] pItem
    [PList.item pItem]
  ]]

  [def-decl pub PList.many_of [-> [type A] [PExpr A] [PList [Array A]]]]
  [def-impl PList.many_of [\ [type A] pItem
    [PList.many [PList.item pItem]]
  ]]

  [def-decl pub PList.some_of [-> [type A] [PExpr A] [PList [Array A]]]]
  [def-impl PList.some_of [\ [type A] pItem
    [PList.some [PList.item pItem]]
  ]]

  [def-decl pub PList.optional_of [-> [type A] [PExpr A] [PList [Maybe A]]]]
  [def-impl PList.optional_of [\ [type A] pItem
    [PList.optional [PList.item pItem]]
  ]]

  [def-decl pub PList.modifier [-> [type A] [PExpr A] [PList Bool]]]
  [def-impl PList.modifier [\ [type A] pExpr
    [PList.optional_of pExpr . map [Maybe.is_some]]
  ]]

  [def-decl pub PList.rest [PList [Array Expr]]]
  [def-impl PList.rest
    [PList.label* [Doc.of "..."] [PList.many_of PExpr.any_expr]]
  ]

  [def-decl pub PList.rest_ [PList ,]]
  [def-impl PList.rest_
    [PList.rest . $> ,]
  ]
]
