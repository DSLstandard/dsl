[file
  [import "base.dsl"]

  // * Church-encoded free monad
  //
  // See https://hackage.haskell.org/package/free/docs/Control-Monad-Trans-Free-Church.html

  [class-decl FreeT [F [-> * *]] [M [-> * *]] [A *]]
  [class-struct FreeT
    // Reference:
    //   newtype FT f m a = FT { runFT :: forall r. (a -> m r) -> (forall x. (x -> m r) -> f x -> m r) -> m r }
    [field run [-> [type R *]
      [-> A [M R]]
      [-> [type X *] [-> X [M R]] [F X] [M R]]
      [M R]
    ]]
  ]

  [tydef Free [-> [-> * *] * *] [\ F [FreeT F Id]]]

  [def-decl pub FreeT.map [-> [type F] [type M] [type A] [type B] [FreeT F M A] [-> A B] [FreeT F M B]]]
  [def-impl FreeT.map [\ [type F] [type M] [type A] [type B] self a->b
    // Reference:
    //   fmap f (FT k) = FT $ \a fr -> k (a . f) fr
    [FreeT.new [\ [type R] b->m.r next->m.r
      [FreeT.run self [a->b . fn.| b->m.r] next->m.r]
    ]]
  ]]

  [def-decl pub FreeT.pure [-> [type F] [type M] [type A] A [FreeT F M A]]]
  [def-impl FreeT.pure [\ [type F] [type M] [type A] a
    // Reference:
    //   pure a = FT $ \k _ -> k a
    [FreeT.new [\ [type R] a->m.r _next->m.r [a->m.r a]]]
  ]]

  [def-decl pub Free.ap [-> [type F] [type M] [type A] [type B] [Free F [-> A B]] [Free F A] [Free F B]]]
  [def-impl Free.ap [\ [type F] [type M] [type A] [type B] f.a->b f.a
    // Reference:
    //   FT fk <*> FT ak = FT $ \b fr -> fk (\e -> ak (\d -> b (e d)) fr) fr
    [FreeT.new [\ [type R *] b->m.r next->m.r
      [FreeT.run f.a->b
        [\ a->b [FreeT.run f.a [a->b . fn.| b->m.r] next->m.r]]
        next->m.r
      ]
    ]]
  ]]

  [def-decl pub FreeT.then [-> [type F] [type M] [type A] [type B] [FreeT F M A] [-> A [FreeT F M B]] [FreeT F M B]]]
  [def-impl FreeT.then [\ [type F] [type M] [type A] [type B] self a->m.b
    // Reference: FT fk >>= f = FT $ \b fr -> fk (\d -> runFT (f d) b fr) fr
    [FreeT.new [\ [type R *] b->m.r next->m.r
      [FreeT.run self
        [\ a [FreeT.run [a->m.b a] b->m.r next->m.r]]
        next->m.r
      ]
    ]]
  ]]

  [def-decl pub auto FreeT.Functor [-> [type F] [type M] [Functor [FreeT F M]]]]
  [def-impl FreeT.Functor [\ [type F] [type M]
    [Functor.new [\ [type A] [type B] [FreeT.map]]]
  ]]

  [def-decl pub auto FreeT.Pure [-> [type F] [type M] [Pure [FreeT F M]]]]
  [def-impl FreeT.Pure [\ [type F] [type M]
    [Pure.new [\ [type A] [FreeT.pure]]]
  ]]

  [def-decl pub auto FreeT.Apply [-> [type F] [type M] [Apply [Free F]]]]
  [def-impl FreeT.Apply [\ [type F] [type M]
    [Apply.new [\ [type A] [type B] [Free.ap]]]
  ]]

  [def-decl pub auto FreeT.Then [-> [type F] [type M] [Then [FreeT F M]]]]
  [def-impl FreeT.Then [\ [type F] [type M]
    [Then.new [\ [type A] [type B] [FreeT.then]]]
  ]]

  // Wrap a Church-encoding of a "free monad" as the free monad for a functor.
  //
  // Useful if you think the Free monad constructor looks too ugly to use directly.
  [def-decl pub Free.free [-> [type F] [type A] [-> [type R] [-> A R] [-> [F R] R] R] [Free F A]]]
  [def-impl Free.free [\ [type F] [type A] church
    // Reference: free f = FT (\kp kf -> return $ f (runIdentity . kp) (runIdentity . kf return))
    [FreeT.new [\ [type R] a->m.r next->m.r
      [church [a->m.r] [next->m.r [fn.id]]]
    ]]
  ]]

  // Like the inverse of 'Free.free'.
  [def-decl pub Free.runF [-> [type F] [type A] [Free F A] [auto [Functor F]] [-> [type R] [-> A R] [-> [F R] R] R]]]
  [def-impl Free.runF [\ [type F] [type A] self [auto F.Functor]
    // Reference: runF (FT m) = \kp kf -> runIdentity $ m (return . kp) (\xg -> return . kf . fmap (runIdentity . xg))
    [\ [type R] a->r f.r->r
      [Id.run [FreeT.run self
        [\ a [a . a->r . Id.new]]
        [\ [type X] x->m.r f.x [f.x . map [\ x [x . x->m.r . Id.run]] . f.r->r . Id.new]]
      ]]
    ]
  ]]

  // Merge a functor layer into the free monad.
  [def-decl pub FreeT.wrap [-> [type F] [type M] [type A] [F [FreeT F M A]] [FreeT F M A]]]
  [def-impl FreeT.wrap [\ [type F] [type M] [type A] f.free
    // Reference:
    //    wrap f = FT (\kp kf -> kf (\ft -> runFT ft kp kf) f)
    [FreeT.new [\ [type R *] a->m.r next->m.r
      [next->m.r [\ [fx [FreeT F M A]] [FreeT.run fx a->m.r next->m.r]] f.free]
    ]]
  ]]

  // * "Rigid" free monad (ThreeT)
  //
  // See
  // https://hackage.haskell.org/package/free/docs/Control-Monad-Trans-Free.html
  //
  // A pattern matchable free monad, which is useful for writing interpreters
  // that need to inspect the structure of the free monad.

  [class-decl ThreeF [F [-> * *]] [R *] [A *]]
  [class-enum ThreeF
    [member Pure R]
    [member Free [F A]]
  ]

  [class-decl ThreeT [F [-> * *]] [M [-> * *]] [A *]]
  [class-struct ThreeT 
    [field run [M [ThreeF F A [ThreeT F M A]]]]
  ]

  [tydef Three [-> [-> * *] * *] [\ F [ThreeT F Id]]]

  [def-decl pub ThreeF.map [-> [type F] [type R] [type A] [type B] [ThreeF F R A] [auto [Functor F]] [-> A B] [ThreeF F R B]]]
  [def-impl ThreeF.map [\ [type F] [type R] [type A] [type B] self [auto F.Functor] a->b
    [match self
      [[Pure r]
        [ThreeF.Pure r]]
      [[Free fa]
        [ThreeF.Free [fa . map a->b]]]
    ]
  ]]

  [def-decl pub ThreeT.map [-> [type F] [type M] [type A] [type B] [ThreeT F M A] [auto [Functor F]] [auto [Functor M]] [-> A B] [ThreeT F M B]]]
  [def-impl ThreeT.map [\ [type F] [type M] [type A] [type B] self [auto F.Functor] [auto M.Functor] a->b
    [ThreeT.new [self . ThreeT.run . map [\ threef [match threef
      [[Pure a]
        [ThreeF.Pure [a->b a]]]
      [[Free ffa]
        [ThreeF.Free [ffa . map [\ fa [ThreeT.map fa a->b]]]]]
    ]]]]
  ]]

  [def-decl pub ThreeT.wrap [-> [type F] [type M] [type A] [F [ThreeT F M A]] [auto [Monad M]] [ThreeT F M A]]]
  [def-impl ThreeT.wrap [\ [type F] [type M] [type A] f.three [auto M.Monad]
    [ThreeT.new [pure [ThreeF.Free f.three]]]
  ]]

  [def-decl pub ThreeT.from_free [-> [type F] [type M] [type A]
    [FreeT F M A] [auto [Functor F]] [auto [Monad M]] [ThreeT F M A]]]
  [def-impl ThreeT.from_free [\ [type F] [type M] [type A] self [auto F.Functor] [auto M.Monad]
    // Reference:
    //   fromFT :: (Monad m, Functor f) => FT f m a -> FreeT f m a
    //   fromFT (FT k) = FreeT $ k (return . Pure) (\xg -> runFreeT . wrap . fmap (FreeT . xg))
    [ThreeT.new [FreeT.run self
      [\ a [pure [ThreeF.Pure a]]]
      [\ [type X] x->m.r f.x [f.x . map [\ x [ThreeT.new [x->m.r x]]] . ThreeT.wrap . ThreeT.run]]
    ]]
  ]]
]