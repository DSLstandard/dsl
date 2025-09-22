[file
  [import "base.dsl"]
  [import "binary_heap.dsl"]
  [import "free.dsl"]

  // Simple A-Star search monad
  //
  // Inspired by https://hackage.haskell.org/package/astar-monad and
  // https://hackage.haskell.org/package/monad-dijkstra.

  [class-decl AStarStep [C *] [R *]]
  [class-enum AStarStep
    // ((G, H) cost updater, next AStar)
    [member UpdateCost [; [-> C C [PairOf C]] R]]

    [member Junction [; R R]]

    [member Abandon ,]

    // For deferred impure actions. We don't rely on the monad transformer
    // machinery of free monads since we want to control when the deferred logic
    // are executed when doing A*.
    [member IO [IO R]]
  ]

  [def-decl AStarStep.map [-> [type C] [type A] [type B] [AStarStep C A] [-> A B] [AStarStep C B]]]
  [def-impl AStarStep.map [\ [type C] [type A] [type B] step a->b
    [match step
      [[UpdateCost [; mod_cost next]] [do
        [AStarStep.UpdateCost mod_cost [a->b next]]
      ]]
      [[Junction [; x1 x2]] [do
        [AStarStep.Junction [a->b x1] [a->b x2]]
      ]]
      [[Abandon] [do
        [AStarStep.Abandon]
      ]]
      [[IO thunk] [do
        [AStarStep.IO [thunk . map a->b]]
      ]]
    ]
  ]]

  [def-decl pub auto AStarStep.Functor [-> [type C] [Functor [AStarStep C]]]]
  [def-impl AStarStep.Functor [\ [type C]
    [Functor.new [\ [type A] [type B] a->b step
      [AStarStep.map a->b step]
    ]]
  ]]

  [tydef AStar [-> * * *] [\ C A [Free [AStarStep C] A]]]

  // Defer the impure action of computing the next AStar step until we explore
  // this path.
  [def-decl pub AStar.lazy [-> [type C] [type A] [-> , [AStar C A]] [AStar C A]]]
  [def-impl AStar.lazy [\ [type C] [type A] thunk
    [FreeT.wrap [AStarStep.IO [IO.new thunk]]]
  ]]

  [def-decl pub AStar.lazily [-> [type C] [AStar C ,]]]
  [def-impl AStar.lazily [\ [type C]
    [AStar.lazy [\ _ [FreeT.pure ,]]]
  ]]

  [def-decl pub AStar.abandon [-> [type C] [type A] [AStar C A]]]
  [def-impl AStar.abandon [\ [type C] [type A] [FreeT.wrap [AStarStep.Abandon]]]]

  // Fork into multiple paths. 'AStar.lazily' is automatically applied in this
  // function for ergonomics.
  [def-decl pub AStar.fork [-> [type C] [type A] [Iter [AStar C A]] [AStar C A]]]
  [def-impl AStar.fork [\ [type C] [type A] iter [do
    [try val [iter . Iter.foldr
      [AStar.abandon]
      [\ branch right [FreeT.wrap [AStarStep.Junction [branch] right]]]
    ]]
    [try _ [AStar.lazily]]
    [pure val]
  ]]]

  [def-decl pub AStar.pick [-> [type C] [type A] [Iter A] [AStar C A]]]
  [def-impl AStar.pick [\ [type C] [type A] iter [do
    [iter . Iter.map [\ x [pure x]] . AStar.fork]
  ]]]

  [def-decl pub AStar.abandon [-> [type C] [type A] [AStar C A]]]
  [def-impl AStar.abandon [\ [type C] [type A] [FreeT.wrap [AStarStep.Abandon]]]]

  // Update the G cost of this path. 'AStar.lazily' is automatically applied in
  // this function for ergonomics.
  [def-decl pub AStar.update_g_cost [-> [type C] [type A] [-> C C] [AStar C ,]]]
  [def-impl AStar.update_g_cost [\ [type C] [type A] mod_g_cost [do
    [try _ [FreeT.wrap [AStarStep.UpdateCost
      [\ g_cost h_cost [; [mod_g_cost g_cost] h_cost]]
      [FreeT.pure ,]
    ]]]
    [AStar.lazily]
  ]]]

  [class-decl AStarHead [C *] [A *]]
  [class-struct AStarHead
    [field three [ThreeT [AStarStep C] Id A]]
    [field g_cost C]
    [field h_cost C]
    [field f_cost C] // Pre-computed for performance'
  ]

  [class-decl AStarParams [C *] [A *]]
  [class-struct AStarParams
    [field cost_+ [-> C C C]]
    [field cost_0 C]

    [field is_cost_< [-> C C Bool]]
    [field seed [AStar C A]]
  ]

  // A simple config for A-Star that uses I32 for costs and a user-provided seed
  // for the initial state.
  [def-decl pub AStarParams.by_i32_cost [-> [type A] [AStar I32 A] [AStarParams I32 A]]]
  [def-impl AStarParams.by_i32_cost [\ [type A] seed [do
    [AStarParams.new* [dict
      [cost_+ I32.+]
      [cost_0 0]
      [is_cost_< [\ x y [< x y]]]
      [seed seed]
    ]]
  ]]]

  // Yields (final G cost, result).
  [def-decl pub AStar.iter [-> [type C] [type A] [AStarParams C A] [Iter [; C A]]]]
  [def-impl AStar.iter [\ [type C] [type A] params [do
    [let compute_f_cost [\ [entry [AStarHead C A]]
      [params . AStarParams.cost_+
        [entry . AStarHead.g_cost]
        [entry . AStarHead.h_cost]
      ]
    ]]

    [let entry_is_< [\ [x1 [AStarHead C A]] [x2 [AStarHead C A]] [do
      [let c1 [x1 . AStarHead.f_cost]]
      [let c2 [x2 . AStarHead.f_cost]]
      [params . AStarParams.is_cost_< c1 c2]
    ]]]

    [let frontier [BinaryHeap.create entry_is_<]]
    [frontier . BinaryHeap.push [AStarHead.new* [dict
      [three [params . AStarParams.seed . ThreeT.from_free]]
      [g_cost [params . AStarParams.cost_0]]
      [h_cost [params . AStarParams.cost_0]]
      [f_cost [params . AStarParams.cost_0]]
    ]]]

    [let*
      [advance_head [\ [head [AStarHead C A]] [do
        [match [head . AStarHead.three . ThreeT.run . Id.run]
          [[Free [IO io]] [do
            [let head* [head . AStarHead.=three [IO.run io]]]
            [advance_head head*]
          ]]
          [[Free [UpdateCost [; mod_cost next]]] [do
            [let= [; new_g_cost new_h_cost]
              [mod_cost [head . AStarHead.g_cost] [head . AStarHead.h_cost]]]
            [let new_f_cost
              [params . AStarParams.cost_+ new_g_cost new_h_cost]]
            [let entry* [AStarHead.new* [dict
              [three next]
              [g_cost new_g_cost]
              [h_cost new_h_cost]
              [f_cost new_f_cost]
            ]]]
            [frontier . BinaryHeap.push entry*]
            [find_next_head ,]
          ]]
          [[Free [Junction [; x1 x2]]] [do
            [let h1 [head . AStarHead.=three x1]]
            [let h2 [head . AStarHead.=three x2]]
            [frontier . BinaryHeap.push h2]
            [advance_head h1]
          ]]
          [[Free [Abandon]] [do
            [find_next_head ,]
          ]]
          [[Pure a] [do
            [let g_cost [head . AStarHead.g_cost]]
            [Some [; g_cost a]]
          ]]
        ]
      ]]]

      [find_next_head [\ _ [do
        [match [frontier . BinaryHeap.pop?]
          [[None] [do
            [None]
          ]]
          [[Some entry] [do
            [advance_head entry]
          ]]
        ]
      ]
    ]]]

    [Iter.new find_next_head]
  ]]]

  [def-decl pub AStar.iter_ [-> [type C] [type A] [AStarParams C A] [Iter A]]]
  [def-impl AStar.iter_ [\ [type C] [type A] params [do
    [AStar.iter params . Iter.map [Pair.snd]]
  ]]]

  [def-decl pub AStar.find_best [-> [type C] [type A] [AStarParams C A] [Maybe [; C A]]]]
  [def-impl AStar.find_best [\ [type C] [type A] params [do
    [Iter.head [AStar.iter params]]
  ]]]

  [def-decl pub AStar.find_best_ [-> [type C] [type A] [AStarParams C A] [Maybe A]]]
  [def-impl AStar.find_best_ [\ [type C] [type A] params [do
    [AStar.find_best params . Maybe.map [Pair.snd]]
  ]]]
]