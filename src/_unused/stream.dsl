[file
  [import "base.dsl"]

  // * Base library: Stream
  //
  // Inspired by https://hackage.haskell.org/package/streaming, with an almost
  // 1-to-1 definition and embraces impurity (e.g., you are allowed to run I/O
  // in pure-looking functions like 'Stream.maps' or 'Stream.filter' from the
  // Haskell 'streaming' package) for the sake of programming convenience.
  //
  // 'M' is commonly set to 'IO' (and you may not even have to run an IO effect
  // and instead directly use impure code in functions like 'Stream.map' or
  // 'Stream.lazy', etc...). However, sometimes you have to set 'M' type
  // parameter to monads like 'Async' (for JavaScript) for asynchronous
  // programming.

  [class-decl Of [V *] [A *]]
  [class-struct Of
    [field current V]
    [field next A]
  ]

  [class-decl Stream [F [-> * *]] [M [-> * *]] [R *]]
  [class-enum Stream
    [member Effect [M [Stream F M R]]]
    [member Step [F [Stream F M R]]]
    [member Lazy [-> , [Stream F M R]]]
    [member Return R]
  ]

  // Beam: A (B)asic Str(eam)
  //
  // This is a convenient alias for the most common kind of stream for impure
  // programming.
  [tydef Beam [-> * * *] [\ A R [Stream [Of A] IO R]]]

  [def-decl pub Stream.then [->
    [type F] [type M] [type A] [type B]
    [Stream F M A] [auto [Functor F]] [auto [Functor M]] [-> A [Stream F M B]]
    [Stream F M B]
  ]]
  [def-impl Stream.then [\ [type F] [type M] [type A] [type B] self [auto F.Functor] [auto M.Functor] a->mb [do
    [match self
      [[Effect next] [Stream.Effect [map next [\ s [Stream.then s a->mb]]]]]
      [[Lazy next] [Stream.Lazy [\ _ [Stream.then [next ,] a->mb]]]]
      [[Step next] [Stream.Step [map next [\ s [Stream.then s a->mb]]]]]
      [[Return a] [a->mb a]]
    ]
  ]]]

  [def-decl pub Stream.pure [-> [type F] [type M] [type R] R [Stream F M R]]]
  [def-impl Stream.pure [\ [type F] [type M] [type R] val [Stream.Return val]]]

  [def-decl pub Stream.map [->
    [type F] [type M] [type A] [type B]
    [Stream F M A] [auto [Functor F]] [auto [Functor M]] [-> A B]
    [Stream F M B]
  ]]
  [def-impl Stream.map [\ [type F] [type M] [type A] [type B] self [auto F.Functor] [auto M.Functor] a->b
    [self . Stream.then [\ a [Stream.pure [a->b a]]]]
  ]]

  [def-decl pub Stream.<*> [->
    [type F] [type M] [type A] [type B]
    [Stream F M [-> A B]] [auto [Functor F]] [auto [Functor M]] [Stream F M A] [Stream F M B]]]
  [def-impl Stream.<*> [\ [type F] [type M] [type A] [type B] f.a->b [auto F.Functor] [auto M.Functor] f.a
    [f.a->b . Stream.then [\ a->b [f.a . Stream.then [\ a [Stream.pure [a->b a]]]]]]
  ]]

  [def-decl pub auto Stream.Functor [-> [type F] [type M] [auto [Functor F]] [auto [Functor M]] [Functor [Stream F M]]]]
  [def-impl Stream.Functor [\ [type F] [type M] [auto F.Functor] [auto M.Functor]
    [Functor.new [\ [type A] [type B] fa a->b [Stream.map fa a->b]]]
  ]]

  [def-decl pub auto Stream.Pure [-> [type F] [type M] [auto [Functor F]] [auto [Functor M]] [Pure [Stream F M]]]]
  [def-impl Stream.Pure [\ [type F] [type M] [auto F.Functor] [auto M.Functor]
    [Pure.new [\ [type A] a [Stream.pure a]]]
  ]]

  [def-decl pub auto Stream.Apply [-> [type F] [type M] [auto [Functor F]] [auto [Functor M]] [Apply [Stream F M]]]]
  [def-impl Stream.Apply [\ [type F] [type M] [auto F.Functor] [auto M.Functor]
    [Apply.new [\ [type A] [type B] ff fa [Stream.<*> ff fa]]]
  ]]

  [def-decl pub auto Stream.Then [-> [type F] [type M] [auto [Functor F]] [auto [Functor M]] [Then [Stream F M]]]]
  [def-impl Stream.Then [\ [type F] [type M] [auto F.Functor] [auto M.Functor]
    [Then.new [\ [type A] [type B] sa a->sb [Stream.then sa a->sb]]]
  ]]

  [def-decl pub Stream.lazy [-> [type F] [type M] [type R] [-> , [Stream F M R]] [Stream F M R]]]
  [def-impl Stream.lazy [\ [type F] [type M] [type R] stream [Stream.Lazy stream]]]

  [def-decl pub Stream.effect [-> [type F] [type M] [type R] [M [Stream F M R]] [Stream F M R]]]
  [def-impl Stream.effect [\ [type F] [type M] [type R] m.stream [Stream.Effect m.stream]]]

  [def-decl pub Stream.wrap [-> [type F] [type M] [type R] [F [Stream F M R]] [Stream F M R]]]
  [def-impl Stream.wrap [\ [type F] [type M] [type R] f.stream [Stream.Step f.stream]]]

  [def-decl pub Stream.cons [-> [type A] [type M] [type R] A [Stream [Of A] M R] [Stream [Of A] M R]]]
  [def-impl Stream.cons [\ [type A] [type M] [type R] v stream [Stream.wrap [Of.new v stream]]]]

  [def-decl pub Stream.yield [-> [type A] [type M] A [Stream [Of A] M ,]]]
  [def-impl Stream.yield [\ [type A] [type M] val [Stream.cons val [Stream.pure ,]]]]

  // Performs all Lazys until Yield or Return is reached.
  [def-decl pub Stream.inspect [-> [type F] [type M] [type R]
    [Stream F M R] [auto [Monad M]] [M [Either R [F [Stream F M R]]]]
  ]]
  [def-impl Stream.inspect [\ [type F] [type M] [type R] self [auto M.Monad]
    [match self
      [[Effect next] [next . then [\ next0 [Stream.inspect next0]]]]
      [[Lazy next] [Stream.inspect [next ,]]]
      [[Step next] [pure [type M] [Right next]]]
      [[Return a] [pure [type M] [Left a]]]
    ]
  ]]

  // Inspect the first item in a stream of elements, without a return value.
  [def-decl pub Stream.uncons [-> [type A] [type M] [type R]
    [Stream [Of A] M R]
    [auto [Monad M]]
    [M [Maybe [; A [Stream [Of A] M R]]]]
  ]]
  [def-impl Stream.uncons [\ [type A] [type M] [type R] self [auto M.Monad]
    [self . Stream.inspect . map [\ out [match out
      [[L r] [None]]
      [[R [Of a stream]] [Some [; a stream]]]
    ]]]
  ]]

  [def-decl pub Stream.zip_with [-> [type A] [type B] [type C] [type M] [type R]
    [Stream [Of A] M R]
    [Stream [Of B] M R]
    [auto [Monad M]]
    [-> A B C]
    [Stream [Of C] M R]
  ]]
  [def-impl Stream.zip_with [\ [type A] [type B] [type C] [type M] [type R] in_xs in_ys [auto M.Monad] combiner [do
    [Stream.effect [do
      [try x_out [in_xs . Stream.inspect]]
      [let= [R [Of x xs]] x_out [[L r] [pure [Stream.pure r]]]]

      [try y_out [in_ys . Stream.inspect]]
      [let= [R [Of y ys]] y_out [[L r] [pure [Stream.pure r]]]]

      [pure [Stream.cons [combiner x y] [Stream.zip_with xs ys combiner]]]
    ]]
  ]]]

  [def-decl pub Stream.zip [-> [type A] [type B]  [type M] [type R]
    [Stream [Of A] M R]
    [Stream [Of B] M R]
    [auto [Monad M]]
    [Stream [Of [; A B]] M R]
  ]]
  [def-impl Stream.zip [\ [type A] [type B] [type M] [type R] in_xs in_ys [auto M.Monad] [do
    [Stream.zip_with in_xs in_ys [\ x y [; x y]]]
  ]]]

  // Generate a stream from an impure producing function.
  [def-decl pub Stream.from_producer [-> [type A] [type M] [type R] [-> , [Either R A]] [Stream [Of A] M R]]]
  [def-impl Stream.from_producer [\ [type A] [type M] [type R] producer
    [Stream.lazy [\ _ [do
      [match [producer ,]
        [[L r] [Stream.pure r]]
        [[R a] [Stream.cons a [Stream.from_producer producer]]]
      ]
    ]]]
  ]]

  [def-decl pub Beam.foreach [-> [type A] [type R] [Beam A R] [-> A ShouldContinue] ,]]
  [def-impl Beam.foreach [\ [type A] [type R] self callback [do
    [match self
      [[Lazy next] [Beam.foreach [next ,] callback]]
      [[Effect next] [Beam.foreach [next . IO.run] callback]]
      [[Step [Of v next]] [do
        [let should-continue [callback v]]
        [if should-continue
          [Beam.foreach next callback]
          ,
        ]
      ]]
      [[Return a] ,]
    ]
  ]]]

  [def-decl pub Beam.foreach* [-> [type A] [type R] [Beam A R] [-> A ,] R]]
  [def-impl Beam.foreach* [\ [type A] [type R] self callback
    [match self
      [[Lazy next] [Beam.foreach* [next ,] callback]]
      [[Effect next] [Beam.foreach* [next . IO.run] callback]]
      [[Step [Of v next]] [do
        [callback v]
        [Beam.foreach* next callback]
      ]]
      [[Return a] a]
    ]
  ]]

  [def-decl pub Beam.uncons [-> [type A] [type R] [Beam A R] [Maybe [; A [Beam A R]]]]]
  [def-impl Beam.uncons [\ [type A] [type R] self
    [self . Stream.uncons . IO.run]
  ]]

  [def-decl pub Stream.repeat [-> [type A] [type M] [type R] A [Stream [Of A] M R]]]
  [def-impl Stream.repeat [\ [type A] [type M] [type R] value
    [Stream.lazy [\ _ [Stream.cons value [Stream.repeat value]]]]
  ]]

  [def-decl pub Stream.range [-> [type M] [Range I32] [Stream [Of I32] M ,]]]
  [def-impl Stream.range [\ [type M] range [do
    [let= [Range start _ step] range]
    [let range_should_continue [Range.mk_should_continue_checker range]]
    [let*
      [go [\ i [
        [if [range_should_continue i]
          [Stream.lazy [\ _ [Stream.cons i [go [i . I32.+ step]]]]]
          [Stream.pure ,]
        ]
      ]]]
    ]
    [go start]
  ]]]

  [def-decl pub Range.stream [-> [type M] [Range I32] [Stream [Of I32] M ,]]]
  [def-impl Range.stream [\ [type M] range [Stream.range range]]]

  [def-decl pub Stream.maps_maybe [->
    [type A] [type B] [type M] [type R]
    [Stream [Of A] M R]
    [auto [Functor M]]
    [-> A [Maybe B]]
    [Stream [Of B] M R]
  ]]
  [def-impl Stream.maps_maybe [\ [type A] [type B] [type M] [type R] self [auto M.Functor] a->b [do
    [Stream.lazy [\ _ [match self
      [[Return r] [Stream.pure r]]
      [[Lazy next] [next , . Stream.maps_maybe a->b]]
      [[Effect next] [Stream.effect [next . map [\ next0 [next0 . Stream.maps_maybe a->b]]]]]
      [[Step [Of x xs]]
        [match [a->b x]
          [[None] [Stream.maps_maybe xs a->b]]
          [[Some y] [Stream.cons y [xs . Stream.maps_maybe a->b]]]
        ]
      ]
    ]]]
  ]]]

  [def-decl pub Stream.maps [->
    [type A] [type B] [type M] [type R]
    [Stream [Of A] M R]
    [auto [Functor M]]
    [-> A B]
    [Stream [Of B] M R]
  ]]
  [def-impl Stream.maps [\ [type A] [type B] [type M] [type R] self [auto M.Functor] a->b
    [self . Stream.maps_maybe [\ a [Some [a->b a]]]]
  ]]

  [def-decl pub Stream.mapsM_maybe [->
    [type A] [type B] [type M] [type R]
    [Stream [Of A] M R]
    [auto [Monad M]]
    [-> A [M [Maybe B]]]
    [Stream [Of B] M R]
  ]]
  [def-impl Stream.mapsM_maybe [\ [type A] [type B] [type M] [type R] self [auto M.Monad] a->m.b
    [Stream.lazy [\ _ [match self
      [[Return r] [Stream.pure r]]
      [[Lazy next] [next , . Stream.mapsM_maybe a->m.b]]
      [[Effect next] [Stream.effect [next . map [\ next0 [next0 . Stream.mapsM_maybe a->m.b]]]]]
      [[Step [Of x xs]] [Stream.effect [
        [a->m.b x] . map [\ b
          [match b
            [[None] [Stream.mapsM_maybe xs a->m.b]]
            [[Some y] [Stream.cons y [xs . Stream.mapsM_maybe a->m.b]]]
          ]
        ]]
      ]]]
    ]]
  ]]

  [def-decl pub Stream.mapsM_ [->
    [type A] [type M] [type R]
    [Stream [Of A] M R]
    [auto [Monad M]]
    [-> A [M ,]]
    [M R]
  ]]
  [def-impl Stream.mapsM_ [\ [type A] [type M] [type R] stream [auto M.Monad] callback [do
    [try result [Stream.inspect stream]]
    [match result
      [[L r] [do
        [pure r]
      ]]
      [[R [Of x xs]] [do
        [try _ [callback x]]
        [Stream.mapsM_ xs callback]
      ]]
    ]
  ]]]

  [def-decl pub Stream.filter [-> [type A] [type B] [type M] [type R] [Stream [Of A] M R] [auto [Functor M]] [-> A Bool] [Stream [Of A] M R]]]
  [def-impl Stream.filter [\ [type A] [type B] [type M] [type R] self [auto M.Functor] predicate
    [Stream.maps_maybe self [\ a [if [predicate a] [Some a] [None]]]]
  ]]

  [def-decl pub Stream.filter_some [-> [type A] [type B] [type M] [type R] [Stream [Of [Maybe A]] M R] [auto [Functor M]] [Stream [Of A] M R]]]
  [def-impl Stream.filter_some [\ [type A] [type B] [type M] [type R] self [auto M.Functor]
    [Stream.maps_maybe self [\ a a]]
  ]]

  // [def-decl pub Stream.concats [-> [type A *] [type B *] [type R *] [Stream [Stream A ,] R] [Stream A R]]]
  // [def-impl Stream.concats [\ [type A *] [type B *] [type R *] self
  //   [Stream.lazy [\ _
  //     [match self
  //       [[Lazy next] [Stream.concats [next ,]]]
  //       [[Cons inner next] [Stream.*> inner [Stream.concats next]]]
  //       [[Return r] [Stream.pure r]]
  //     ]
  //   ]]
  // ]]

  // [def-decl pub Stream.mealy [-> [type A *] [type B *] [type S *] [type R *] [Stream A R] S [-> S A [; S B]] [Stream B R]]]
  // [def-impl Stream.mealy [\ [type A *] [type B *] [type S *] [type R *] self s0 step
  //   [Stream.lazy [\ _ [match self
  //     [[Lazy next] [Stream.mealy [next ,] s0 step]]
  //     [[Cons x next] [do
  //       [let= [; s1 y] [step s0 x]]
  //       [Stream.cons y [Stream.mealy next s1 step]]
  //     ]]
  //     [[Return r] [Stream.pure r]]
  //   ]]]
  // ]]

  // [def-decl pub Stream.ixedFrom [-> [type A *] [type R *] [Stream A R] I32 [Stream [; I32 A] R]]]
  // [def-impl Stream.ixedFrom [\ [type A *] [type R *] self i
  //   [Stream.mealy self i [\ i a [; [i . I32.+ 1] [; i a]]]]
  // ]]

  // [def-decl pub Stream.ixed [-> [type A *] [type R *] [Stream A R] [Stream [; I32 A] R]]]
  // [def-impl Stream.ixed [\ [type A *] [type R *] self
  //   [Stream.ixedFrom self 0]
  // ]]

  [def-decl pub Stream.tee [-> [type A] [type M] [type R] [Stream [Of A] M R] [auto [Functor M]] [-> A ,] [Stream [Of A] M R]]]
  [def-impl Stream.tee [\ [type A] [type M] [type R] self [auto M.Functor] callback
    [Stream.lazy [\ _ [match self
      [[Lazy next] [Stream.tee [next ,] callback]]
      [[Effect next] [Stream.effect [next . map [\ next0 [Stream.tee next0 callback]]]]]
      [[Step [Of x xs]] [do
        [callback x]
        [Stream.cons x [Stream.tee xs callback]]
      ]]
      [[Return r] [Stream.pure r]]
    ]]]
  ]]

  [def-decl pub Beam.drain [-> [type A] [type R] [Beam A R] R]]
  [def-impl Beam.drain [\ [type A] [type R] self
    [self . Beam.foreach* [\ _ ,]]
  ]]

  [def-decl pub Beam.drain_ [-> [type A] [type R] [Beam A R] ,]]
  [def-impl Beam.drain_ [\ [type A] [type R] self [do
    [let _ [self . Beam.drain]]
    ,
  ]]]

  [def-decl pub Beam.inspect [-> [type A] [type R] [Beam A R] [Either R [; A [Beam A R]]]]]
  [def-impl Beam.inspect [\ [type A] [type R] self [do
    [match [self . Stream.inspect . IO.run]
      [[L r] [Left r]]
      [[R [Of x xs]] [Right [; x xs]]]
    ]
  ]]]

  [def-decl pub Beam.zip_with [->
    [type A] [type B] [type C] [type R]
    [Beam A R]
    [Beam B R]
    [-> A B C]
    [Beam C R]]
  ]
  [def-impl Beam.zip_with [\ [type A] [type B] [type C] [type R] xs ys combiner
    [Stream.zip_with xs ys combiner]
  ]]

  [def-decl pub Beam.zip [->
    [type A] [type B] [type R]
    [Beam A R]
    [Beam B R]
    [Beam [; A B] R]
  ]]
  [def-impl Beam.zip [\ [type A] [type B] [type R] xs ys
    [Beam.zip_with xs ys [\ x y [; x y]]]
  ]]

  [def-decl pub Beam.find_some [->
    [type A] [type B]
    [Beam A ,] [-> A [Maybe B]] [Maybe B]
  ]]
  [def-impl Beam.find_some [\ [type A] [type B] stream predicate [do
    [match [Beam.uncons stream]
      [[None] [None]]
      [[Some [; a rest]] [do
        [match [predicate a]
          [[None] [Beam.find_some rest predicate]]
          [[Some b] [Some b]]
        ]
      ]]
    ]
  ]]]

  [def-decl pub Beam.find [-> [type A] [Beam A ,] [-> A Bool] [Maybe A]]]
  [def-impl Beam.find [\ [type A] stream predicate [do
    [Beam.find_some stream [\ a [if [predicate a] [Some a] [None]]]]
  ]]]

  [def-decl pub Stream.split_at [->
    [type F] [type M] [type R]
    [Stream F M R]
    [auto [Functor F]]
    [auto [Functor M]]
    I32
    [Stream F M [Stream F M R]]
  ]]
  [def-impl Stream.split_at [\ [type F] [type M] [type R] self [auto F.Functor] [auto M.Functor] n
    [Stream.lazy [\ _ [match self
      [[Lazy next] [Stream.split_at [next ,] n]]
      [[Effect next] [Stream.Effect [next . map [\ next0 [next0 . Stream.split_at n]]]]]
      [[Step next]
        [if [n . == 0]
          [Stream.pure [Stream.Step next]]
          [Stream.Step [next . map [\ next0 [next0 . Stream.split_at [n . I32.- 1]]]]]
        ]
      ]
      [[Return r] [Stream.pure [Stream.Return r]]]
    ]]]
  ]]

  [def-decl pub Stream.take [->
    [type F] [type M] [type R]
    [Stream F M R] [auto [Functor F]] [auto [Functor M]] I32
    [Stream F M ,]
  ]]
  [def-impl Stream.take [\ [type F] [type M] [type R] self [auto F.Functor] [auto M.Functor] n
    [self . Stream.split_at n . void]
  ]]

  [def-decl pub Stream.drop [-> [type F] [type M] [type R] [Stream F M R] [auto [Functor F]] [auto [Functor M]] I32 [Stream F M R]]]
  [def-impl Stream.drop [\ [type F] [type M] [type R] self [auto F.Functor] [auto M.Functor] n
    [self . Stream.split_at n . flatten]
  ]]

  // [def-decl pub Stream.span [-> [type V *] [type A *] [Stream V A] [-> V Bool] [Stream V [Stream V A]]]]
  // [def-impl Stream.span [\ [type V *] [type A *] self predicate
  //   [Stream.lazy [\ _ [match self
  //     [[Lazy next] [Stream.span [next ,] predicate]]
  //     [[Cons v next]
  //       [if [predicate v]
  //         [Stream.cons v [Stream.span next predicate]]
  //         [Stream.cons v [Stream.pure next]]
  //       ]
  //     ]
  //     [[Return r] [Stream.pure [Stream.Return r]]]
  //   ]]]
  // ]]

  // [def-decl pub Stream.take_while [-> [type V *] [type A *] [Stream V A] [-> V Bool] [Stream V ,]]]
  // [def-impl Stream.take_while [\ [type V *] [type A *] self predicate [do
  //   [self . Stream.span predicate . Stream.void]
  // ]]]

  // [def-decl pub Stream.drop_while [-> [type V *] [type A *] [Stream V A] [-> V Bool] [Stream V A]]]
  // [def-impl Stream.drop_while [\ [type V *] [type A *] self predicate [do
  //   [self . Stream.span predicate . Stream.join]
  // ]]]
]