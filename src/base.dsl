[file
  // * Base library: Basic functions

  [def-decl pub fn.flip [-> [type A *] [type B *] [type C *] [-> A B C] [-> B A C]]]
  [def-impl fn.flip [\ [type A *] [type B *] [type C *] f a b [f b a]]]

  [def-decl pub fn.id [-> [type A *] A A]]
  [def-impl fn.id [\ [type A *] a a]]

  [def-decl pub fn.const [-> [type A *] [type B *] A B A]]
  [def-impl fn.const [\ [type A *] [type B *] a _b a]]

  [def-decl pub fn.const_ [-> [type A *] [type B *] A B B]]
  [def-impl fn.const_ [\ [type A *] [type B *] _a b b]]

  [def-decl pub fn.> [-> [type A *] [type B *] [type C *] [-> A B] [-> B C] [-> A C]]]
  [def-impl fn.> [\ [type A *] [type B *] [type C *] f g a [g [f a]]]]

  [def-decl pub fn.< [-> [type A *] [type B *] [type C *] [-> B C] [-> A B] [-> A C]]]
  [def-impl fn.< [\ [type A *] [type B *] [type C *] g f a [g [f a]]]]

  // Alias to 'fn.>'. Looks like the Unix shell pipe operator.
  [def-decl pub fn.| [-> [type A *] [type B *] [type C *] [-> A B] [-> B C] [-> A C]]]
  [def-impl fn.| fn.>]

  // Alias to 'fn.<'. Looks like the '.' operator in Haskell.
  [def-decl pub fn.o [-> [type A *] [type B *] [type C *] [-> B C] [-> A B] [-> A C]]]
  [def-impl fn.o fn.<]

  // Like Haskell's 'void' but for impure actions.
  [def-decl pub unit [-> [type A *] A ,]]
  [def-impl unit [\ [type A *] _a ,]]

  // Like Haskell's 'Control.Monad.when', but for impure code.
  [def-decl pub when [-> Bool [-> , ,] ,]]
  [def-impl when [\ cond on_true
    [if cond [on_true ,] ,]
  ]]

  // Like Haskell's 'Control.Monad.unless', but for impure code.
  [def-decl pub unless [-> Bool [-> , ,] ,]]
  [def-impl unless [\ cond on_false
    [if cond , [on_false ,]]
  ]]

  [def-decl pub apply_when [-> [type A *] A Bool [-> A A] A]]
  [def-impl apply_when [\ [type A *] self cond f
    [if cond [f self] self]
  ]]

  [def-decl pub apply_unless [-> [type A *] A Bool [-> A A] A]]
  [def-impl apply_unless [\ [type A *] self cond f
    [if cond self [f self]]
  ]]

  // Like Idris2's 'the' function. Used to supply type annotations in places
  // where type inference fails.
  [def-decl pub the [-> [type A] A A]]
  [def-impl the [\ [type A] a a]]

  // Alias to 'the'. Has a nice syntax like Haskell.
  [def-decl pub :: [-> [type A] A A]]
  [def-impl :: [\ [type A] a a]]

  // * Base library: Bool

  [def-decl pub Bool.not [-> Bool Bool]]
  [def-impl Bool.not [\ x [if x false true]]]

  [def-decl pub Bool.and [-> Bool Bool Bool]]
  [def-impl Bool.and [\ x y [if x y false]]]

  [def-decl pub Bool.or [-> Bool Bool Bool]]
  [def-impl Bool.or [\ x y [if x true y]]]

  [def-decl pub Bool.to_str [-> Bool String]]
  [def-impl Bool.to_str [\ bool [if bool "true" "false"]]]

  [def-decl pub Bool.== [-> Bool Bool Bool]]
  [def-impl Bool.== [\ x y [if x y [Bool.not y]]]]

  [def-decl pub Bool.!= [-> Bool Bool Bool]]
  [def-impl Bool.!= [\ x y [if x [Bool.not y] y]]]

  [def-decl pub Bool.xor [-> Bool Bool Bool]]
  [def-impl Bool.xor Bool.!=]

  [def-decl pub Bool.xnor [-> Bool Bool Bool]]
  [def-impl Bool.xnor Bool.==]

  // * Base library: Basic String functions

  [def-decl pub String.<> [-> String String String]]
  [def-annotate String.<> "foreign-import:string_concat"]

  // * Base library: Semigroup Monoid

  [class-decl Semigroup [A *]]
  [class-struct Semigroup
    [field <> [-> A A A]]
  ]

  [def-decl pub <> [-> [type A] A [auto [Semigroup A]] A A]]
  [def-impl <> [\ [type A] x [auto A.Semigroup] y [Semigroup.<> A.Semigroup x y]]]

  [class-decl Neutral [A *]]
  [class-struct Neutral
    [field neutral A]
  ]

  [def-decl pub neutral [-> [type A] [auto [Neutral A]] A]]
  [def-impl neutral [\ [type A] [auto A.Neutral] [A.Neutral . Neutral.neutral]]]

  [tydef Monoid [-> * *] [\ M [; [Semigroup M] [Neutral M]]]]

  // * Base library: Functor

  [class-decl Functor [F [-> * *]]]
  [class-struct Functor
    [field map [-> [type A] [type B] [F A] [-> A B] [F B]]]
  ]

  [def-decl pub map [-> [type F] [type A] [type B] [F A] [auto [Functor F]] [-> A B] [F B]]]
  [def-impl map [\ [type F] [type A] [type B] fa [auto F.Functor] a->b [F.Functor . Functor.map fa a->b]]]

  [def-decl pub <$> [-> [type F] [type A] [type B] [-> A B] [F A] [auto [Functor F]] [F B]]]
  [def-impl <$> [\ [type F] [type A] [type B] a->b fa [auto F.Functor] [F.Functor . Functor.map fa a->b]]]

  [def-decl pub $> [-> [type F] [type A] [type B] [F A] [auto [Functor F]] B [F B]]]
  [def-impl $> [\ [type F] [type A] [type B] fa [auto F.Functor] b [fa . map [\ _ b]]]]

  [def-decl pub void [-> [type F] [type A] [F A] [auto [Functor F]] [F ,]]]
  [def-impl void [\ [type F] [type A] fa [auto F.Functor] [fa . $> ,]]]

  // * Base library: Applicative

  [class-decl Pure [F [-> * *]]]
  [class-struct Pure
    [field pure [-> [type A] A [F A]]]
  ]

  [def-decl pub pure [-> [type F] [type A] A [auto [Pure F]] [F A]]]
  [def-impl pure [\ [type F] [type A] a [auto F.Pure] [F.Pure . Pure.pure a]]]

  // Like Haskell's
  // 'https://hackage.haskell.org/package/relude/docs/Relude-Applicative.html#v:pass'
  //
  // Probably inspired by Python's 'pass' keyword.
  [def-decl pub pass [-> [type F] [auto [Pure F]] [F ,]]]
  [def-impl pass [\ [type F] [auto F.Pure] [pure ,]]]

  [class-decl Apply [F [-> * *]]]
  [class-struct Apply
    [field <*> [-> [type A] [type B] [F [-> A B]] [F A] [F B]]]
  ]

  [tydef Applicative [-> [-> * *] *] [\ F [; [Functor F] [Pure F] [Apply F]]]]

  [def-decl pub <*> [-> [type F] [type A] [type B] [F [-> A B]] [auto [Apply F]] [F A] [F B]]]
  [def-impl <*> [\ [type F] [type A] [type B] f.a->b [auto F.Apply] f.a [[F.Apply . Apply.<*>] f.a->b f.a]]]

  [def-decl pub <* [-> [type F] [type A] [type B] [F A] [auto [Applicative F]] [F B] [F A]]]
  [def-impl <* [\ [type F] [type A] [type B] f.a [auto F.Applicative] f.b [f.a . map [fn.const] . <*> f.b]]]

  [def-decl pub *> [-> [type F] [type A] [type B] [F A] [auto [Applicative F]] [F B] [F B]]]
  [def-impl *> [\ [type F] [type A] [type B] f.a [auto F.Applicative] f.b [f.a . map [fn.flip [fn.const]] . <*> f.b]]]

  // * Base library: Monad

  [class-decl Then [M [-> * *]]]
  [class-struct Then
    [field then [-> [type A] [type B] [M A] [-> A [M B]] [M B]]]
  ]

  [tydef Monad [-> [-> * *] *] [\ F [; [Then F] [Applicative F]]]]

  // NOTE: '[auto [Then M]]' must be placed at the end for instance resolution
  // to work correctly for the '[try _ ...]' syntax.
  [def-decl pub then [-> [type M] [type A] [type B] [M A] [-> A [M B]] [auto [Then M]] [M B]]]
  [def-impl then [\ [type M] [type A] [type B] ma a->mb [auto M.Then] [M.Then . Then.then ma a->mb]]]

  [def-decl pub flatten [-> [type M] [type A] [M [M A]] [auto [Then M]] [M A]]]
  [def-impl flatten [\ [type M] [type A] m.ma [auto M.Then] [m.ma . then [fn.id]]]]

  // * Base library: Eq

  [class-decl Eq [A *]]
  [class-struct Eq
    [field == [-> A A Bool]]
  ]

  [def-decl pub == [-> [type A *] A [auto [Eq A]] A Bool]]
  [def-impl == [\ [type A *] a [auto eq] b [eq . Eq.== a b]]]

  [def-decl pub != [-> [type A *] A [auto [Eq A]] A Bool]]
  [def-impl != [\ [type A *] a [auto eq] b [Bool.not [eq . Eq.== a b]]]]

  [def-decl pub auto Eq.Unit [Eq ,]]
  [def-impl Eq.Unit [Eq.new [\ _ _ true]]]

  [def-decl pub auto Eq.Bool [Eq Bool]]
  [def-impl Eq.Bool [Eq.new Bool.==]]

  // * Base library: Ord

  [class-decl Ordering]
  [class-enum Ordering
    [member LT ,]
    [member EQ ,]
    [member GT ,]
  ]

  [def-decl pub auto Ordering.Eq [Eq Ordering]]
  [def-impl Ordering.Eq [Eq.new [\ x y
    [match [; x y]
      [[; [LT] [LT]] true]
      [[; [EQ] [EQ]] true]
      [[; [GT] [GT]] true]
      [_ false]
    ]
  ]]]

  [def-decl pub Ordering.is_== [-> Ordering Bool]]
  [def-impl Ordering.is_== [== Ordering.EQ]]

  [def-decl pub Ordering.is_!= [-> Ordering Bool]]
  [def-impl Ordering.is_!= [!= Ordering.EQ]]

  [def-decl pub Ordering.is_< [-> Ordering Bool]]
  [def-impl Ordering.is_< [== Ordering.LT]]

  [def-decl pub Ordering.is_> [-> Ordering Bool]]
  [def-impl Ordering.is_> [== Ordering.GT]]

  [def-decl pub Ordering.is_<= [-> Ordering Bool]]
  [def-impl Ordering.is_<= [!= Ordering.GT]]

  [def-decl pub Ordering.is_>= [-> Ordering Bool]]
  [def-impl Ordering.is_>= [!= Ordering.LT]]

  [def-decl pub Ordering.flip [-> Ordering Ordering]]
  [def-impl Ordering.flip [\ ord [match ord
    [[LT] Ordering.GT]
    [[EQ] Ordering.EQ]
    [[GT] Ordering.LT]
  ]]]

  [class-decl Ord [A *]]
  [class-struct Ord
    [field on_compare [-> A A Ordering]]
  ]

  [def-decl pub Ord.compare [-> [type A *] A A [auto [Ord A]] Ordering]]
  [def-impl Ord.compare [\ [type A *] a b [auto A.Ord] [A.Ord . Ord.on_compare a b]]]

  [def-decl pub < [-> [type A *] A A [auto [Ord A]] Bool]]
  [def-impl < [\ [type A *] a b [auto A.Ord] [Ord.compare a b . Ordering.is_<]]]

  [def-decl pub > [-> [type A *] A A [auto [Ord A]] Bool]]
  [def-impl > [\ [type A *] a b [auto A.Ord] [Ord.compare a b . Ordering.is_>]]]

  [def-decl pub >= [-> [type A *] A A [auto [Ord A]] Bool]]
  [def-impl >= [\ [type A *] a b [auto A.Ord] [Ord.compare a b . Ordering.is_>=]]]

  [def-decl pub <= [-> [type A *] A A [auto [Ord A]] Bool]]
  [def-impl <= [\ [type A *] a b [auto A.Ord] [Ord.compare a b . Ordering.is_<=]]]

  [def-decl pub Ord.max [-> [type A *] A A [auto [Ord A]] A]]
  [def-impl Ord.max [\ [type A *] a b [auto ord] [if [< a b] b a]]] 

  [def-decl pub Ord.min [-> [type A *] A A [auto [Ord A]] A]]
  [def-impl Ord.min [\ [type A *] a b [auto ord] [if [< a b] a b]]]

  // * Base library: Unit implementations

  [def-decl pub auto Unit.Semigroup [Semigroup ,]]
  [def-impl Unit.Semigroup [Semigroup.new [\ _ _ ,]]]

  [def-decl pub auto Unit.Neutral [Neutral ,]]
  [def-impl Unit.Neutral [Neutral.new ,]]

  // * Base library: Endo function monoid
  //
  // Endo f <> Endo g == Endo (f . g)

  [class-decl Endo [A *]]
  [class-struct Endo
    [field run [-> A A]]
  ]

  [def-decl pub auto Endo.Semigroup [-> [type A] [Semigroup [Endo A]]]]
  [def-impl Endo.Semigroup [\ [type A] [Semigroup.new [\ f g
    [Endo.new [\ x [Endo.run f [Endo.run g x]]]]
  ]]]]

  [def-decl pub auto Endo.Neutral [-> [type A] [Neutral [Endo A]]]]
  [def-impl Endo.Neutral [\ [type A]
    [Neutral.new [Endo.new [fn.id]]]
  ]]

  // * Base library: Dual monoid

  [class-decl Dual [A *]]
  [class-struct Dual [field unwrap A]]

  [def-decl pub auto Dual.Semigroup [-> [type A] [auto [Semigroup A]] [Semigroup [Dual A]]]]
  [def-impl Dual.Semigroup [\ [type A] [auto A.Semigroup] [Semigroup.new
    [\ x y [Dual.new [[Dual.unwrap y] . <> [Dual.unwrap x]]]]
  ]]]

  [def-decl pub auto Dual.Neutral [-> [type A] [auto [Neutral A]] [Neutral [Dual A]]]]
  [def-impl Dual.Neutral [\ [type A] [auto A.Neutral]
    [Neutral.new [Dual.new [neutral]]]
  ]]

  // Base library: Alternative

  [class-decl AEmpty [F [-> * *]]]
  [class-struct AEmpty
    [field aempty [-> [type A] [F A]]]
  ]

  [def-decl pub aempty [-> [type F] [type A] [auto [AEmpty F]] [F A]]]
  [def-impl aempty [\ [type F] [type A] [auto F.AEmpty] [F.AEmpty . AEmpty.aempty]]]

  [class-decl APlus [F [-> * *]]]
  [class-struct APlus
    [field <|> [-> [type A] [F A] [F A] [F A]]]
  ]

  [def-decl pub <|> [-> [type F] [type A] [F A] [auto [APlus F]] [F A] [F A]]]
  [def-impl <|> [\ [type F] [type A] x [auto F.APlus] y [F.APlus . APlus.<|> x y]]]

  [tydef Alt [-> [-> * *] *] [\ F [; [AEmpty F] [APlus F] [Applicative F]]]]

  [def-decl pub Alt.with_default [-> [type F] [type A] [F A] [auto [Alt F]] A [F A]]]
  [def-impl Alt.with_default [\ [type F] [type A] f.a [auto F.Alt] default
    [f.a . <|> [pure default]]
  ]]

  // * Base library: Foldable

  [class-decl Foldable [F [-> * *]]]
  [class-struct Foldable
    [field on_reduce [-> [type A] [type S] [F A] S [-> S A S] S]]
  ]

  [def-decl pub Foldable.reduce [-> [type F] [type A] [type S] [F A] [auto [Foldable F]] S [-> S A S] S]]
  [def-impl Foldable.reduce [\ [type F] [type A] [type S] f.a [auto F.Foldable] state steps
    [F.Foldable . Foldable.on_reduce f.a state steps]
  ]]

  // Impurely iterates over the elements of the Foldable.
  //
  // NOTE: It is impossible to implement a version of '.foreach' that has
  // 'break'/'continue'.
  [def-decl pub Foldable.foreach [-> [type F] [type A] [F A] [auto [Foldable F]] [-> A ,] ,]]
  [def-impl Foldable.foreach [\ [type F] [type A] f.a [auto F.Foldable] step
    [f.a . Foldable.reduce , [\ _ a [step a]]]
  ]]

  [def-decl pub Foldable.sconcat_map [-> [type F] [type A] [type S] [F A] [auto [Foldable F]] S [-> A S] [auto [Semigroup S]] S]]
  [def-impl Foldable.sconcat_map [\ [type F] [type A] [type S] f.a [auto F.Foldable] state f [auto S.Semigroup]
    [f.a . Foldable.reduce state [\ s a [s . <> [f a]]]]
  ]]

  [def-decl pub Foldable.sconcat [-> [type F] [type A] [F A] [auto [Foldable F]] [auto [Semigroup A]] A A]]
  [def-impl Foldable.sconcat [\ [type F] [type A] f.a [auto F.Foldable] [auto A.Semigroup] state
    [Foldable.sconcat_map f.a state [fn.id]]
  ]]

  [def-decl pub Foldable.mconcat_map [-> [type F] [type A] [type S] [F A] [auto [Foldable F]] [-> A S] [auto [Monoid S]] S]]
  [def-impl Foldable.mconcat_map [\ [type F] [type A] [type S] f.a [auto F.Foldable] f [auto S.Monoid]
    [Foldable.sconcat_map f.a [neutral] f]
  ]]

  [def-decl pub Foldable.mconcat [-> [type F] [type A] [F A] [auto [Foldable F]] [auto [Monoid A]] A]]
  [def-impl Foldable.mconcat [\ [type F] [type A] f.a [auto F.Foldable] [auto A.Monoid]
    [Foldable.mconcat_map f.a [fn.id]]
  ]]

  // Alias to 'reduce'
  [def-decl pub Foldable.foldl [-> [type F] [type A] [type S] [F A] [auto [Foldable F]] S [-> S A S] S]]
  [def-impl Foldable.foldl [\ [type F] [type A] [type S] [Foldable.reduce]]]

  [def-decl pub Foldable.foldr [-> [type F] [type A] [type S] [F A] [auto [Foldable F]] S [-> A S S] S]]
  [def-impl Foldable.foldr [\ [type F] [type A] [type S] f.a [auto F.Foldable] state step
    [f.a . Foldable.mconcat_map [\ a [Endo.new [step a]]] . Endo.run state]
  ]]

  [def-decl pub Foldable.asum [-> [type F] [type A] [type T] [T [F A]] [auto [Foldable T]] [auto [Alt F]] [F A]]]
  [def-impl Foldable.asum [\ [type F] [type A] [type T] t.f.a [auto T.Foldable] [auto F.Alt]
    [t.f.a . Foldable.foldr [aempty] [\ x acc [x . <|> acc]]]
  ]]

  // Aborts the program with an error message.
  //
  // Can be used like Go's 'panic()' or Rust's 'panic!' or Haskell's 'error',
  // etc...
  [def-decl pub panic [-> [type A *] String A]]
  [def-annotate panic "foreign-import:panic"]

  // Dumps the value to the console and returns it. Useful for debugging.
  [def-decl pub echo [-> [type A *] A ,]]
  [def-annotate echo "foreign-import:echo"]

  // * Base library: Bytes

  [def-decl pub Bytes.length [-> Bytes I32]]
  [def-annotate Bytes.length "foreign-import:bytes_length"]

  [def-decl pub Bytes.at [-> Bytes I32 U8]]
  [def-annotate Bytes.at "foreign-import:bytes_at"]

  // * Base library: I32

  [def-decl pub I32.== [-> I32 I32 Bool]]
  [def-annotate I32.== "foreign-import:i32_equal"]

  [def-decl pub auto Eq.I32 [Eq I32]]
  [def-impl Eq.I32 [Eq.new I32.==]]

  [def-decl pub I32.compare [-> [type A] I32 I32 A A A A]]
  [def-annotate I32.compare "foreign-import:i32_compare"]

  [def-decl pub auto I32.Ord [Ord I32]]
  [def-impl I32.Ord [Ord.new
    [\ a b [I32.compare a b Ordering.LT Ordering.EQ Ordering.GT]]
  ]]

  [def-decl pub I32.+ [-> I32 I32 I32]]
  [def-annotate I32.+ "foreign-import:i32_add"]

  [def-decl pub I32.- [-> I32 I32 I32]]
  [def-annotate I32.- "foreign-import:i32_sub"]

  [def-decl pub I32.* [-> I32 I32 I32]]
  [def-annotate I32.* "foreign-import:i32_mul"]

  [def-decl pub I32.negate [-> I32 I32]]
  [def-annotate I32.negate "foreign-import:i32_negate"]

  [def-decl pub I32.floordiv [-> I32 I32 I32]]
  [def-annotate I32.floordiv "foreign-import:i32_floordiv"]

  [def-decl pub I32.mod [-> I32 I32 I32]]
  [def-annotate I32.mod "foreign-import:i32_mod"]

  [def-decl pub I32.is_divisible_by [-> I32 I32 Bool]]
  [def-impl I32.is_divisible_by [\ self denom [self . I32.mod denom . == 0]]]

  [def-decl pub I32.to_str [-> I32 String]]
  [def-annotate I32.to_str "foreign-import:i32_to_str"]

  [def-decl pub I32.is_even [-> I32 Bool]]
  [def-impl I32.is_even [\ x [ x . I32.mod 2 . == 0 ]]]

  [def-decl pub I32.is_odd [-> I32 Bool]]
  [def-impl I32.is_odd [\ x [ x . I32.mod 2 . == 1 ]]]

  [def-decl pub I32.min [-> I32 I32 I32]]
  [def-impl I32.min [\ x y [if [x . < y] x y]]]

  [def-decl pub I32.max [-> I32 I32 I32]]
  [def-impl I32.max [\ x y [if [x . > y] x y]]]

  [def-decl pub I32.suc [-> I32 I32]]
  [def-impl I32.suc [\ x [x . I32.+ 1]]]

  [def-decl pub I32.dec [-> I32 I32]]
  [def-impl I32.dec [\ x [x . I32.- 1]]]

  // * Base library: Int

  [def-decl pub Int.== [-> Int Int Bool]]
  [def-annotate Int.== "foreign-import:int_equal"]

  [def-decl pub Int.compare [-> [type A] Int Int A A A A]]
  [def-annotate Int.compare "foreign-import:int_compare"]

  [def-decl pub Int.+ [-> Int Int Int]]
  [def-annotate Int.+ "foreign-import:int_add"]

  [def-decl pub Int.- [-> Int Int Int]]
  [def-annotate Int.- "foreign-import:int_sub"]

  [def-decl pub Int.* [-> Int Int Int]]
  [def-annotate Int.* "foreign-import:int_mul"]

  [def-decl pub Int.clamp_to_i32 [-> Int I32]]
  [def-annotate Int.clamp_to_i32 "foreign-import:int_clamp_to_i32"]

  [def-decl pub Int.from_i32 [-> I32 Int]]
  [def-annotate Int.from_i32 "foreign-import:int_from_i32"]

  [def-decl pub Int.negate [-> Int Int]]
  [def-annotate Int.negate "foreign-import:int_negate"]

  [def-decl pub Int.mod [-> Int Int Int]]
  [def-annotate Int.mod "foreign-import:int_mod"]

  [def-decl pub Int.to_str [-> Int String]]
  [def-annotate Int.to_str "foreign-import:int_to_string"]

  // StringBuilder

  [class-decl StringBuilder]

  [def-decl pub StringBuilder.create [-> , StringBuilder]]
  [def-annotate StringBuilder.create "foreign-import:string_builder_create"]

  [def-decl pub StringBuilder.append [-> StringBuilder String ,]]
  [def-annotate StringBuilder.append "foreign-import:string_builder_append_string"]

  [def-decl pub StringBuilder.append_char [-> StringBuilder Char ,]]
  [def-annotate StringBuilder.append_char "foreign-import:string_builder_append_char"]

  [def-decl pub StringBuilder.build [-> StringBuilder String]]
  [def-annotate StringBuilder.build "foreign-import:string_builder_build"]

  // * Base library: Void

  [class-decl Void]
  [class-enum Void]

  [def-decl Void.absurd [-> [type A *] Void A]]
  [def-impl Void.absurd [\ [type A *] _void [panic "IMPOSSIBLE: Void.absurd is called"]]]

  // * Base library: Maybe

  [class-decl Maybe [A *]]
  [class-enum Maybe
    [member None ,]
    [member Some A]
  ]

  [def-decl pub Some [-> [type A *] A [Maybe A]]]
  [def-impl Some [\ [type A *] value [Maybe.Some value]]]

  [def-decl pub None [-> [type A *] [Maybe A]]]
  [def-impl None [\ [type A *] [Maybe.None]]]

  [def-decl pub Maybe.is_some [-> [type A *] [Maybe A] Bool]]
  [def-impl Maybe.is_some [\ [type A *] input
    [match input
      [[Some _] true]
      [[None] false]
    ]
  ]]

  [def-decl pub Maybe.is_none [-> [type A *] [Maybe A] Bool]]
  [def-impl Maybe.is_none [\ [type A *] input
    [match input
      [[Some _] false]
      [[None] true]
    ]
  ]]

  [def-decl pub Maybe.map [-> [type A *] [type B *] [Maybe A] [-> A B] [Maybe B]]]
  [def-impl Maybe.map [\ [type A *] [type B *] input a->b
    [match input
      [[Some value] [Some [a->b value]]]
      [[None] [None]]
    ]
  ]]

  [def-decl pub Maybe.pure [-> [type A *] A [Maybe A]]]
  [def-impl Maybe.pure [\ [type A *] a [Some a]]]

  [def-decl pub Maybe.then [-> [type A *] [type B *] [Maybe A] [-> A [Maybe B]] [Maybe B]]]
  [def-impl Maybe.then [\ [type A *] [type B *] ma a->mb
    [match ma
      [[Some a] [a->mb a]]
      [[None] [None]]
    ]
  ]]

  [def-decl pub Maybe.<*> [-> [type A *] [type B *] [Maybe [-> A B]] [Maybe A] [Maybe B]]]
  [def-impl Maybe.<*> [\ [type A *] [type B *] a->b a
    [match a
      [[Some a] [match a->b
        [[Some f] [Some [f a]]]
        [[None] [None]]
      ]]
      [[None] [None]]
    ]
  ]]

  [def-decl pub auto Maybe.Functor [Functor Maybe]]
  [def-impl Maybe.Functor [Functor.new Maybe.map]]

  [def-decl pub auto Maybe.Pure [Pure Maybe]]
  [def-impl Maybe.Pure [Pure.new Maybe.pure]]

  [def-decl pub auto Maybe.Apply [Apply Maybe]]
  [def-impl Maybe.Apply [Apply.new Maybe.<*>]]

  [def-decl pub auto Maybe.Then [Then Maybe]]
  [def-impl Maybe.Then [Then.new Maybe.then]]

  [def-decl pub Maybe.expect* [-> [type A *] [Maybe A] [-> , String] A]]
  [def-impl Maybe.expect* [\ [type A *] input on_none
    [match input
      [[Some value] value]
      [[None] [panic ["Maybe.expect: " . String.<> [on_none ,]]]]
    ]
  ]]

  [def-decl pub Maybe.expect [-> [type A *] [Maybe A] String A]]
  [def-impl Maybe.expect [\ [type A *] input none_msg
    [Maybe.expect* input [\ _ none_msg]]
  ]]

  [def-decl pub Maybe.unwrap [-> [type A *] [Maybe A] A]]
  [def-impl Maybe.unwrap [\ [type A *] input
    [match input
      [[Some value] value]
      [[None] [panic "Maybe.unwrap: called on None"]]
    ]
  ]]

  [def-decl pub Maybe.unwrap_or* [-> [type A *] [Maybe A] [-> , A] A]]
  [def-impl Maybe.unwrap_or* [\ [type A *] input default
    [match input
      [[Some value] value]
      [[None] [default ,]]
    ]
  ]]

  [def-decl pub Maybe.unwrap_or [-> [type A *] [Maybe A] A A]]
  [def-impl Maybe.unwrap_or [\ [type A *] input default
    [match input
      [[Some value] value]
      [[None] default]
    ]
  ]]

  [def-decl pub Maybe.when_some [-> [type A *] [Maybe A] [-> A ,] ,]]
  [def-impl Maybe.when_some [\ [type A *] input on_some
    [match input
      [[Some value] [on_some value]]
      [[None] ,]
    ]
  ]]

  [def-decl pub Maybe.guard [-> Bool [Maybe ,]]]
  [def-impl Maybe.guard [\ condition
    [if condition [Some ,] [None]]
  ]]

  [def-decl pub Alt.optional [-> [type F] [type A] [F A] [auto [Alt F]] [F [Maybe A]]]]
  [def-impl Alt.optional [\ [type F] [type A] f.a [auto F.Alt]
    [<|> [f.a . map [Some]] [pure [None]]]
  ]]

  // If you have `["a", "b", "c", "d"]` with a combiner `[\ x y [x + ',' + y]]`,
  // you get `a,b,c,d`.
  [def-decl pub Foldable.foldl1? [-> [type F] [type A] [F A] [auto [Foldable F]] [-> A A A] [Maybe A]]]
  [def-impl Foldable.foldl1? [\ [type F] [type A] f.a [auto F.Foldable] combine
    [f.a . Foldable.foldl [None] [\ acc y
      [Some [match acc
        [[None] y]
        [[Some x] [combine x y]]
      ]]
    ]]
  ]]

  [def-decl pub Foldable.foldl1 [-> [type F] [type A] [F A] [auto [Foldable F]] [-> A A A] A]]
  [def-impl Foldable.foldl1 [\ [type F] [type A] f.a [auto F.Foldable] combine
    [match [Foldable.foldl1? f.a combine]
      [[Some result] result]
      [[None] [panic "Foldable.foldl1 called on empty Foldable"]]
    ]
  ]]

  [def-decl pub Foldable.intercalate? [-> [type F] [type A] [F A] [auto [Foldable F]] [auto [Semigroup A]] A [Maybe A]]]
  [def-impl Foldable.intercalate? [\ [type F] [type A] f.a [auto F.Foldable] [auto A.Semigroup] sep
    [Foldable.foldl1? f.a [A.Semigroup . Semigroup.<>]]
  ]]

  [def-decl pub Foldable.intercalate [-> [type F] [type A] [F A] [auto [Foldable F]] [auto [Monoid A]] A A]]
  [def-impl Foldable.intercalate [\ [type F] [type A] f.a [auto F.Foldable] [auto A.Monoid] sep
    [Foldable.intercalate? f.a sep . Maybe.unwrap_or [neutral [type A]]]
  ]]

  // * Base library: Either

  [class-decl Either [E *] [A *]]
  [class-enum Either
    [member L E]
    [member R A]
  ]

  [def-decl pub Left [-> [type E] [type A] E [Either E A]]]
  [def-impl Left [\ [type E] [type A] err [Either.L err]]]

  [def-decl pub Right [-> [type E] [type A] A [Either E A]]]
  [def-impl Right [\ [type E] [type A] value [Either.R value]]]

  [def-decl pub auto Either.Functor [-> [type E] [Functor [Either E]]]]
  [def-impl Either.Functor [\ [type E] [Functor.new
    [type [Either E]]
    [\ [type A] [type B] m.a a->b [match m.a
      [[L err] [Left err]]
      [[R a] [Right [a->b a]]]
    ]]
  ]]]

  [def-decl pub auto Either.Pure [-> [type E] [Pure [Either E]]]]
  [def-impl Either.Pure [\ [type E] [Pure.new
    [type [Either E]]
    [\ [type A] a [Right a]]
  ]]]

  [def-decl pub auto Either.Apply [-> [type E] [Apply [Either E]]]]
  [def-impl Either.Apply [\ [type E] [Apply.new
    [type [Either E]]
    [\ [type A] [type B] m.a->b m.a [do
      [match m.a->b
        [[L err] [Left err]]
        [[R a->b] [match m.a
          [[L err] [Left err]]
          [[R a] [Right [a->b a]]]
        ]]
      ]
    ]]
  ]]]

  [def-decl pub auto Either.Then [-> [type E] [Then [Either E]]]]
  [def-impl Either.Then [\ [type E] [Then.new
    [type [Either E]]
    [\ [type A] [type B] ma a->mb [do
      [match ma
        [[L err] [Left err]]
        [[R a] [a->mb a]]
      ]
    ]]
  ]]]

  [def-decl pub Either.is_left [-> [type E *] [type A *] [Either E A] Bool]]
  [def-impl Either.is_left [\ [type E *] [type A *] self
    [match self
      [[L _] true]
      [[R _] false]
    ]
  ]]

  [def-decl pub Either.is_right [-> [type E *] [type A *] [Either E A] Bool]]
  [def-impl Either.is_right [\ [type E *] [type A *] self
    [match self
      [[L _] false]
      [[R _] true]
    ]
  ]]

  [def-decl pub Either.expect* [-> [type E *] [type A *] [Either E A] [-> , String] A]]
  [def-impl Either.expect* [\ [type E *] [type A *] self on_fail
    [match self
      [[L _] [panic ["Either.expect: " . String.<> [on_fail ,]]]]
      [[R value] value]
    ]
  ]]

  [def-decl pub Either.expect [-> [type E *] [type A *] [Either E A] String A]]
  [def-impl Either.expect [\ [type E *] [type A *] self fail_msg
    [self . Either.expect* [\ _ fail_msg]]
  ]]

  [def-decl pub Either.unwrap [-> [type E] [type A] [Either E A] A]]
  [def-impl Either.unwrap [\ [type E] [type A] result
    [match result
      [[L err] [panic "Either.unwrap called on L"]]
      [[R value] value]
    ]
  ]]

  [def-decl pub Either.unwrap_or [-> [type E *] [type A *] [Either E A] A A]]
  [def-impl Either.unwrap_or [\ [type E *] [type A *] self default
    [match self
      [[L _] default]
      [[R value] value]
    ]
  ]]

  // * Base library: Pair

  [tydef PairOf [-> * *] [\ A [; A A]]]

  [def-decl pub Pair.new [-> [type A] [type B] A B [; A B]]]
  [def-impl Pair.new [\ [type A] [type B] a b [; a b]]]

  [def-decl pub Pair.fst [-> [type A] [type B] [; A B] A]]
  [def-impl Pair.fst [\ [type A] [type B] input
    [match input [[; a _] a]]
  ]]

  [def-decl pub Pair.snd [-> [type A] [type B] [; A B] B]]
  [def-impl Pair.snd [\ [type A] [type B] input
    [match input [[; _ b] b]]
  ]]

  [def-decl pub Pair.=fst [-> [type A] [type B] [type X] [; A X] B [; B X]]]
  [def-impl Pair.=fst [\ [type A] [type B] [type X] input x
    [match input [[; _ y] [; x y]]]
  ]]

  [def-decl pub Pair.=snd [-> [type A] [type B] [type X] [; X A] B [; X B]]]
  [def-impl Pair.=snd [\ [type A] [type B] [type X] input y
    [match input [[; x _] [; x y]]]
  ]]

  [def-decl pub Pair.map_fst [-> [type A] [type B] [type X] [; A X] [-> A B] [; B X]]]
  [def-impl Pair.map_fst [\ [type A] [type B] [type X] input f
    [match input [[; x y] [; [f x] y]]]
  ]]

  [def-decl pub Pair.map_snd [-> [type A] [type B] [type X] [; X A] [-> A B] [; X B]]]
  [def-impl Pair.map_snd [\ [type A] [type B] [type X] input f
    [match input [[; x y] [; x [f y]]]]
  ]]

  [def-decl pub Pair.swap [-> [type A] [type B] [; A B] [; B A]]]
  [def-impl Pair.swap [\ [type A] [type B] input
    [match input [[; a b] [; b a]]]
  ]]

  [def-decl pub Pair.map [-> [type X] [type A] [type B] [; X A] [-> A B] [; X B]]]
  [def-impl Pair.map [\ [type X] [type A] [type B] input a->b [do
    [let= [; x a] input]
    [; x [a->b a]]
  ]]]

  // * Base library: Tuple utils

  [def-decl pub Tuple.two [-> [type A] [type B] A B [; A B]]]
  [def-impl Tuple.two [\ [type A] [type B] a b [; a b]]]

  [def-decl pub Tuple.three [-> [type A] [type B] [type C] A B C [; A B C]]]
  [def-impl Tuple.three [\ [type A] [type B] [type C] a b c [; a b c]]]

  [def-decl pub Tuple.four [-> [type A] [type B] [type C] [type D] A B C D [; A B C D]]]
  [def-impl Tuple.four [\ [type A] [type B] [type C] [type D] a b c d [; a b c d]]]

  [def-decl pub Tuple.five [-> [type A] [type B] [type C] [type D] [type E] A B C D E [; A B C D E]]]
  [def-impl Tuple.five [\ [type A] [type B] [type C] [type D] [type E] a b c d e [; a b c d e]]]

  [def-decl pub Tuple.six [-> [type A] [type B] [type C] [type D] [type E] [type F] A B C D E F [; A B C D E F]]]
  [def-impl Tuple.six [\ [type A] [type B] [type C] [type D] [type E] [type F] a b c d e f [; a b c d e f]]]

  // * Base library: These

  [class-decl These [A *] [B *]]
  [class-enum These
    [member L A]
    [member R B]
    [member T [; A B]]
  ]

  [def-decl pub These.from_either [-> [type A *] [type B *] [Either A B] [These A B]]]
  [def-impl These.from_either [\ [type A *] [type B *] either
    [match either
      [[L a] [These.L a]]
      [[R b] [These.R b]]
    ]
  ]]

  [def-decl pub These.coalesce [-> [type A *] [These A A] [-> A A A] A]]
  [def-impl These.coalesce [\ [type A *] input coalesce
    [match input
      [[L a] a]
      [[R a] a]
      [[T [; a _]] a]
    ]
  ]]

  // * Base library: Semialign
  //
  // See https://hackage.haskell.org/package/semialign-1.3.1/docs/Data-Semialign.html

  [class-decl Semialign [F [-> * *]]]
  [class-struct Semialign
    [field on_align_with [-> [type A] [type B] [type C] [F A] [F B] [-> [These A B] C] [F C]]]
  ]

  [def-decl pub Semialign.align_with [-> [type F] [type A] [type B] [type C] [F A] [F B] [auto [Semialign F]] [-> [These A B] C] [F C]]]
  [def-impl Semialign.align_with [\ [type F] [type A] [type B] [type C] fa fb [auto F.Semialign] f
    [F.Semialign . Semialign.on_align_with fa fb f]
  ]]

  [def-decl pub Semialign.align [-> [type F] [type A] [type B] [F A] [F B] [auto [Semialign F]] [F [These A B]]]]
  [def-impl Semialign.align [\ [type F] [type A] [type B] fa fb [auto F.Semialign]
    [Semialign.align_with fa fb [fn.id]]
  ]]

  // * Base library: Looping

  [tydef ShouldContinue * Bool]

  [def-decl pub break ShouldContinue]
  [def-impl break false]

  [def-decl pub continue ShouldContinue]
  [def-impl continue true]

  [def-decl pub loop [-> [-> , ShouldContinue] ,]]
  [def-impl loop [\ callback [do
    [let should-continue [callback ,]]
    [if should-continue
      [loop callback]
      ,
    ]
  ]]]

  [def-decl pub loop_some [-> [type A *] [-> , [Maybe A]] A]]
  [def-impl loop_some [\ [type A *] callback [do
    [let result [callback ,]]
    [match result
      [[Some value] value]
      [[None] [loop_some callback]]
    ]
  ]]]

  // * Base library: Ref

  [class-decl Ref [A *]]

  [def-decl pub Ref.create [-> [type A *] A [Ref A]]]
  [def-annotate Ref.create "foreign-import:ref_create"]

  [def-decl pub Ref.get [-> [type A *] [Ref A] A]]
  [def-annotate Ref.get "foreign-import:ref_get"]

  [def-decl pub Ref.set [-> [type A *] [Ref A] A ,]]
  [def-annotate Ref.set "foreign-import:ref_set"]

  // * Base library: Mut

  [class-decl Mut [A *]]
  [class-struct Mut
    [field setter [-> A ,]]
    [field getter [-> , A]]
  ]

  [def-decl pub var [-> [type A *] A [Mut A]]]
  [def-impl var [\ [type A *] value [do
    [let ref [Ref.create value]]
    [Mut.new
      [Ref.set ref]
      [\ _ [Ref.get ref]]
    ]
  ]]]

  [def-decl pub get [-> [type A *] [Mut A] A]]
  [def-impl get [\ [type A *] mut [mut . Mut.getter ,]]]

  [def-decl pub set [-> [type A *] [Mut A] A ,]]
  [def-impl set [\ [type A *] mut value [Mut.setter mut value]]]

  [def-decl pub update [-> [type A *] [Mut A] [-> A A] ,]]
  [def-impl update [\ [type A *] mut f [do
    [set mut [f [get mut]]]
  ]]]

  // * Base library: Thunk

  [def-decl pub thunk [-> [type A] [-> , A] , A]]
  [def-impl thunk [\ [type A] compute [do
    [let cached [var [None [type A]]]]
    [\ _ [do
      [match [get cached]
        [[None] [do
          [let value [compute ,]]
          [set cached [Some value]]
          value
        ]]
        [[Some value] [do
          value
        ]]
      ]
    ]]
  ]]]

  // * Base library: Identity functor
  //
  // Named 'Id' (like Scalaz) instead 'Identity' (like Haskell) because it is
  // very common.

  [class-decl Id [A *]]
  [class-struct Id
    [field run A]
  ]

  [def-decl pub auto Id.Functor [Functor Id]]
  [def-impl Id.Functor [Functor.new [\ [type A] [type B] m f [Id.new [f [Id.run m]]]]]]

  [def-decl pub auto Id.Apply [Apply Id]]
  [def-impl Id.Apply [Apply.new [\ [type A] [type B] mf ma
    [Id.new [[Id.run mf] [Id.run ma]]]
  ]]]

  [def-decl pub auto Id.Pure [Pure Id]]
  [def-impl Id.Pure [Pure.new [\ [type A] a [Id.new a]]]]

  [def-decl pub auto Id.Then [Then Id]]
  [def-impl Id.Then [Then.new [\ [type A] [type B] ma a->mb [a->mb [Id.run ma]]]]]

  // * Base library: Const functor
  //
  // Like Haskell's 'Const'.

  [class-decl Const [R *] [A *]]
  [class-struct Const
    [field unwrap R]
  ]

  [def-decl pub auto Const.Functor [-> [type R] [Functor [Const R]]]]
  [def-impl Const.Functor [\ [type R] [Functor.new
    [\ [type A] [type B] m.a _a->b [Const.new [Const.unwrap m.a]]]
  ]]]

  [def-decl pub auto Const.Apply [-> [type R] [auto [Semigroup R]] [Apply [Const R]]]]
  [def-impl Const.Apply [\ [type R] [auto R.Semigroup] [Apply.new
    [\ [type A] [type B] m.a->b m.a [do
      [let x [Const.unwrap m.a->b]]
      [let y [Const.unwrap m.a]]
      [Const.new [x . <> y]]
    ]]
  ]]]

  [def-decl pub auto Const.Pure [-> [type R] [auto [Neutral R]] [Pure [Const R]]]]
  [def-impl Const.Pure [\ [type R] [auto R.Neutral] [Pure.new
    [\ [type A] _a [Const.new [neutral]]]
  ]]]

  // * Base library: IO monad
  //
  // Literally defined as '[IO A] == [-> , A]'.
  //
  // In other words, 'IO' is a delayed action.
  //
  // You don't have to use this monad for I/O operations, since the language is
  // impure. However, you may need to fit a 'Monad M' somewhere if a
  // functionality needs a monad type.

  [class-decl IO [A *]]
  [class-struct IO
    [field action [-> , A]]
  ]

  [def-decl pub IO.run [-> [type A *] [IO A] A]]
  [def-impl IO.run [\ [type A *] io [IO.action io ,]]]

  [def-decl pub IO.pure [-> [type A *] A [IO A]]]
  [def-impl IO.pure [\ [type A *] a
    [IO.new [\ _ a]]
  ]]

  // Chains two 'IO' actions together. Impure actions are NOT executed until you
  // call 'IO.run'.
  [def-decl pub IO.then [-> [type A *] [type B *] [IO A] [-> A [IO B]] [IO B]]]
  [def-impl IO.then [\ [type A *] [type B *] self a->io.b
    [IO.new [\ _ [self . IO.run . a->io.b . IO.run]]]
  ]]

  [def-decl pub IO.map [-> [type A *] [type B *] [IO A] [-> A B] [IO B]]]
  [def-impl IO.map [\ [type A *] [type B *] self a->b
    [IO.then self [\ a [IO.pure [a->b a]]]]
  ]]

  [def-decl pub IO.<*> [-> [type A *] [type B *] [IO [-> A B]] [IO A] [IO B]]]
  [def-impl IO.<*> [\ [type A *] [type B *] io.a->b io.a
    [IO.then io.a->b [\ a->b [IO.then io.a [\ a [IO.pure [[a->b a]]]]]]]
  ]]

  [def-decl pub auto IO.Functor [Functor IO]]
  [def-impl IO.Functor [Functor.new IO.map]]

  [def-decl pub auto IO.Pure [Pure IO]]
  [def-impl IO.Pure [Pure.new IO.pure]]

  [def-decl pub auto IO.Apply [Apply IO]]
  [def-impl IO.Apply [Apply.new IO.<*>]]

  [def-decl pub auto IO.Then [Then IO]]
  [def-impl IO.Then [Then.new IO.then]]

  // * Base library: Iter
  //
  // An impure stream with '.next()' (there is no '.has_next()' or '.peek()',
  // you have to create your own API over 'Iter').
  //
  // The most powerful implementation of a stream is
  // 'https://hackage.haskell.org/package/streaming', but 'Iter' is meant to
  // only be a simple handle for iterating through common data structures like
  // 'Array', 'I32Map', etc.
  //
  // While 'Iter' is an inferior implementation of
  // 'https://hackage.haskell.org/package/streaming', it still has a lot of very
  // powerful combinators that you can use.

  [class-decl Iter [A *]]
  [class-struct Iter 
    [field on_next? [-> , [Maybe A]]]
  ]

  [def-decl pub Iter.next? [-> [type A] [Iter A] [Maybe A]]]
  [def-impl Iter.next? [\ [type A] self
    [Iter.on_next? self ,]
  ]]

  [def-decl pub Iter.foreach [-> [type A] [Iter A] [-> A ShouldContinue] ,]]
  [def-impl Iter.foreach [\ [type A] self callback [do
    [loop [\ _ [do
      [match [self . Iter.next?]
        [[None] break]
        [[Some a] [callback a]]
      ]
    ]]]
  ]]]

  [def-decl pub Iter.map [-> [type A] [type B] [Iter A] [-> A B] [Iter B]]]
  [def-impl Iter.map [\ [type A] [type B] self a->b
    [Iter.new [\ _ [do
      [self . Iter.next? . map a->b]
    ]]]
  ]]

  [def-decl pub auto Iter.Functor [Functor Iter]]
  [def-impl Iter.Functor [Functor.new Iter.map]]

  // Returns the first 'Some' and stops iterating, returns 'None' if there is no
  // 'Some'.
  [def-decl pub Iter.first_some [-> [type A] [Iter [Maybe A]] [Maybe A]]]
  [def-impl Iter.first_some [\ [type A] self [do
    [let result [var [None]]]
    [self . Iter.foreach [\ a [do
      [if [Maybe.is_some a]
        [do [set result a] break]
        continue
      ]
    ]]]
    [get result]
  ]]]

  // Like 'Iter.first_some', but first applies a function to each item.
  [def-decl pub Iter.find_some [-> [type A] [type B] [Iter A] [-> A [Maybe B]] [Maybe B]]]
  [def-impl Iter.find_some [\ [type A] [type B] self a->mb
    [self . Iter.map a->mb . Iter.first_some]
  ]]

  [def-decl pub Iter.map_maybe [-> [type A] [type B] [Iter A] [-> A [Maybe B]] [Iter B]]]
  [def-impl Iter.map_maybe [\ [type A] [type B] self a->mb [do
    [let* [go [\ _ [do
      [match [self . Iter.next?]
        [[None] [None]]
        [[Some a] [do
          [match [a->mb a]
            [[None] [go ,]] // Read until an item passes the 'a->mb' test
            [[Some b] [Some b]
          ]]
        ]]
      ]
    ]]]]
    [Iter.new go]
  ]]]

  [def-decl pub Iter.filter [-> [type A] [Iter A] [-> A Bool] [Iter A]]]
  [def-impl Iter.filter [\ [type A] self predicate [do
    [let* [go [\ _ [do
      [match [self . Iter.next?]
        [[None] [None]]
        [[Some a] [if [predicate a] [Some a] [go ,]]]
      ]
    ]]]]
    [Iter.new go]
  ]]]

  [def-decl pub Iter.filter_some [-> [type A] [Iter [Maybe A]] [Iter A]]]
  [def-impl Iter.filter_some [\ [type A] self [self . Iter.map_maybe [fn.id]]]]

  // Walk through the whole iterator monadically and does not return anything.
  //
  // If you want to collect the results, use monadic actions or mutable state
  // with impure code inside the callback.
  [def-decl pub Iter.traverse_ [-> [type A] [type B] [type M] [Iter A] [auto [Monad M]] [-> A [M ShouldContinue]] [M ,]]]
  [def-impl Iter.traverse_ [\ [type A] [type B] [type M] self [auto M.Monad] callback [do
    [let* [go [\ _ [:: [type [M ,]] [do
      [match [self . Iter.next?]
        [[None] [pure ,]]
        [[Some a] [do
          [try should_continue [callback a]]
          [if should_continue [go ,] [pure ,]]
        ]]
      ]
    ]]]]]
    [go ,]
  ]]]

  [def-decl pub Iter.mealy [-> [type A] [type B] [type S] [Iter A] S [-> S A [; S B]] [Iter B]]]
  [def-impl Iter.mealy [\ [type A] [type B] [type S] self initial_state step [do
    [let state [var initial_state]]
    [Iter.new [\ _ [do
      [match [self . Iter.next?]
        [[None] [None]]
        [[Some a] [do
          [let= [; s* b] [step [get state] a]]
          [set state s*]
          [Some b]
        ]]
      ]
    ]]]
  ]]]

  // Like Python's 'enumerate(<iter>, <start>)'.
  [def-decl pub Iter.ixed_from [-> [type A] [Iter A] I32 [Iter [; I32 A]]]]
  [def-impl Iter.ixed_from [\ [type A] self initial_ix
    [Iter.mealy self initial_ix [\ i x [; [i . I32.+ 1] [; i x]]]]
  ]]

  // Like Python's 'enumerate(<iter>)'.
  [def-decl pub Iter.ixed [-> [type A] [Iter A] [Iter [; I32 A]]]]
  [def-impl Iter.ixed [\ [type A] self [Iter.ixed_from self 0]]]

  [def-decl pub Iter.zip_with [-> [type A] [type B] [type C] [Iter A] [Iter B] [-> A B C] [Iter C]]]
  [def-impl Iter.zip_with [\ [type A] [type B] [type C] as bs combine
    [Iter.new [\ _ [do
      [let= [Some a] [as . Iter.next?]
        [_ [None]]]
      [let= [Some b] [bs . Iter.next?]
        [_ [None]]]
      [Some [combine a b]]
    ]]]
  ]]

  [def-decl pub Iter.zip [-> [type A] [type B] [Iter A] [Iter B] [Iter [; A B]]]]
  [def-impl Iter.zip [\ [type A] [type B] as bs
    [Iter.zip_with as bs [Pair.new]]
  ]]

  [def-decl pub Iter.reduce [-> [type A] [type S] [Iter A] S [-> S A S] S]]
  [def-impl Iter.reduce [\ [type A] [type S] self init step [do
    [let state [var init]]
    [self . Iter.foreach [\ a [do
      [set state [step [get state] a]]
      continue
    ]]]
    [get state]
  ]]]

  [def-decl pub Iter.tee [-> [type A] [Iter A] [-> A ,] [Iter A]]]
  [def-impl Iter.tee [\ [type A] self callback [do
    [let* [go [\ _ [do
      [match [self . Iter.next?]
        [[None] [None]]
        [[Some a] [do
          [callback a]
          [Some a]
        ]]
      ]
    ]]]]
    [Iter.new go]
  ]]]

  [def-decl pub Iter.empty [-> [type A] [Iter A]]]
  [def-impl Iter.empty [\ [type A]
    [Iter.new [\ _ [None]]]
  ]]

  // Alias for 'Iter.empty'.
  [def-decl pub Iter.nil [-> [type A] [Iter A]]]
  [def-impl Iter.nil Iter.empty]

  [def-decl pub Iter.one [-> [type A] A [Iter A]]]
  [def-impl Iter.one [\ [type A] a [do
    [let emitted [var false]]
    [Iter.new [\ _ [do
      [if [get emitted]
        [None]
        [do
          [set emitted true]
          [Some a]
        ]
      ]
    ]]]
  ]]]

  [class-decl Iter.two.State]
  [class-enum Iter.two.State
    [member On1 ,]
    [member On2 ,]
    [member End ,]
  ]

  [def-decl pub Iter.two [-> [type A] A A [Iter A]]]
  [def-impl Iter.two [\ [type A] v1 v2 [do
    [let state [var [Iter.two.State.On1]]]
    [Iter.new [\ _ [do
      [match [get state]
        [[On1] [do
          [set state [Iter.two.State.On2]]
          [Some v1]
        ]]
        [[On2] [do
          [set state [Iter.two.State.End]]
          [Some v2]
        ]]
        [[End] [do
          [None]
        ]]
      ]
    ]]]
  ]]]

  [class-decl Iter.chain_all.State [A *]]
  [class-enum Iter.chain_all.State
    [member Begin ,]
    [member Current [Iter A]]
    [member End ,]
  ]

  [def-decl pub Iter.chain_all [-> [type A] [Iter [Iter A]] [Iter A]]]
  [def-impl Iter.chain_all [\ [type A] self [do
    [let state [var [Iter.chain_all.State.Begin]]]

    [let get_next_iter [\ _ [do
      [match [self . Iter.next?]
        [[None] [do
          [set state [Iter.chain_all.State.End]]
        ]]
        [[Some iter] [do
          [set state [Iter.chain_all.State.Current iter]]
        ]]
      ]
    ]]]

    [let* [step [\ _ [do
      [match [get state]
        [[Begin] [do
          [get_next_iter ,]
          [step ,]
        ]]
        [[Current iter] [do
          [match [iter . Iter.next?]
            [[None] [do
              [get_next_iter ,]
              [step ,]
            ]]
            [[Some value] [do
              [Some value]
            ]]
          ]
        ]]
        [[End] [do
          [None]
        ]]
      ]
    ]]]]

    [Iter.new step]
  ]]]

  [def-decl pub Iter.chain [-> [type A] [Iter A] [Iter A] [Iter A]]]
  [def-impl Iter.chain [\ [type A] curr next
    [Iter.two curr next . Iter.chain_all]
  ]]

  [def-decl pub Iter.<> [-> [type A] [Iter A] [Iter A] [Iter A]]]
  [def-impl Iter.<> Iter.chain]

  [def-decl pub auto Iter.Semigroup [-> [type A] [Semigroup [Iter A]]]]
  [def-impl Iter.Semigroup [\ [type A] [Semigroup.new [Iter.<>]]]]

  [def-decl pub auto Iter.Neutral [-> [type A] [Neutral [Iter A]]]]
  [def-impl Iter.Neutral [\ [type A] [Neutral.new [Iter.empty]]]]

  [def-decl pub Iter.foldl1? [-> [type A] [Iter A] [-> A A A] [Maybe A]]]
  [def-impl Iter.foldl1? [\ [type A] self combine
    [self . Iter.reduce [None] [\ acc x
      [match acc
        [[None] [Some x]]
        [[Some acc] [Some [combine acc x]]]
      ]
    ]]
  ]]

  [def-decl pub Iter.foldl1 [-> [type A] [Iter A] [-> A A A] A]]
  [def-impl Iter.foldl1 [\ [type A] self combine
    [match [Iter.foldl1? self combine]
      [[Some result] result]
      [[None] [panic "Iter.foldl1 called on empty Iter"]]
    ]
  ]]

  [def-decl pub Iter.intercalate? [-> [type A] [Iter A] [auto [Semigroup A]] A [Maybe A]]]
  [def-impl Iter.intercalate? [\ [type A] self [auto A.Semigroup] sep
    [Iter.foldl1? self [A.Semigroup . Semigroup.<>]]
  ]]

  [def-decl pub Iter.intercalate [-> [type A] [Iter A] [auto [Monoid A]] A A]]
  [def-impl Iter.intercalate [\ [type A] self [auto A.Monoid] sep
    [Iter.intercalate? self sep . Maybe.unwrap_or [neutral [type A]]]
  ]]

  [def-decl pub Iter.prepend [-> [type A] [Iter A] A [Iter A]]]
  [def-impl Iter.prepend [\ [type A] self a
    [Iter.chain [Iter.one a] self]
  ]]

  [def-decl pub Iter.append [-> [type A] [Iter A] A [Iter A]]]
  [def-impl Iter.append [\ [type A] self a
    [Iter.chain self [Iter.one a]]
  ]]

  [def-decl pub Iter.foldr [-> [type A] [type S] [Iter A] S [-> A S S] S]]
  [def-impl Iter.foldr [\ [type A] [type S] self init step
    [match [self . Iter.next?]
      [[None] init]
      [[Some a] [step a [Iter.foldr self init step]]]
    ]
  ]]

  [def-decl pub Iter.head [-> [type A] [Iter A] [Maybe A]]]
  [def-impl Iter.head [\ [type A] self [
    [self . Iter.next?]
  ]]]

  // Returns 'false' and stops the iterator once a 'false' is found.
  //
  // If 'Iter' ends or is empty, 'true' is returned.
  [def-decl pub Iter.and [-> [Iter Bool] Bool]]
  [def-impl Iter.and [\ self
    [match [self . Iter.next?]
      [[None] true]
      [[Some a] [if a [Iter.and self] false]]
    ]
  ]]

  // Returns 'true' and stops the iterator once a 'true' is found.
  //
  // If 'Iter' ends or is empty, 'false' is returned.
  [def-decl pub Iter.or [-> [Iter Bool] Bool]]
  [def-impl Iter.or [\ self
    [match [self . Iter.next?]
      [[None] false]
      [[Some a] [if a true [Iter.or self]]]
    ]
  ]]

  // Maps then does 'Iter.and'.
  [def-decl pub Iter.all [-> [type A] [Iter A] [-> A Bool] Bool]]
  [def-impl Iter.all [\ [type A] self predicate
    [self . Iter.map predicate . Iter.and]
  ]]

  // Maps then does 'Iter.or'.
  [def-decl pub Iter.any [-> [type A] [Iter A] [-> A Bool] Bool]]
  [def-impl Iter.any [\ [type A] self predicate
    [self . Iter.map predicate . Iter.or]
  ]]

  // Returns the maximum element of the 'Iter' unless the iterator is empty.
  [def-decl pub Iter.max? [-> [type A] [Iter A] [auto [Ord A]] [Maybe A]]]
  [def-impl Iter.max? [\ [type A] self [auto A.Ord]
    [Iter.foldl1? self [\ state x [Ord.max state x]]]
  ]]

  [def-decl pub Iter.max [-> [type A] [Iter A] [auto [Ord A]] A]]
  [def-impl Iter.max [\ [type A] self [auto A.Ord]
    [match [Iter.max? self]
      [[Some result] result]
      [[None] [panic "Iter.max called on empty Iter"]]
    ]
  ]]

  // Returns the minimum element of the 'Iter' unless the iterator is empty.
  [def-decl pub Iter.min? [-> [type A] [Iter A] [auto [Ord A]] [Maybe A]]]
  [def-impl Iter.min? [\ [type A] self [auto A.Ord]
    [Iter.foldl1? self [\ state x [Ord.min state x]]]
  ]]

  [def-decl pub Iter.min [-> [type A] [Iter A] [auto [Ord A]] A]]
  [def-impl Iter.min [\ [type A] self [auto A.Ord]
    [match [Iter.min? self]
      [[Some result] result]
      [[None] [panic "Iter.min called on empty Iter"]]
    ]
  ]]

  // Drain the iterator and apply each function onto the initial value. Returns
  // the final value.
  //
  // If the iterator is empty, the initial value is returned.
  [def-decl pub Iter.apply_onto [-> [type A] [Iter [-> A A]] A A]]
  [def-impl Iter.apply_onto [\ [type A] self a
    [self . Iter.reduce a [\ acc f [f acc]]]
  ]]

  // * Base library: Gen
  //
  // A monad that can be used to implement generators (e.g., JavaScript
  // generators / Python generators / or even Kotlin suspending functions).
  //
  // You can turn a 'Gen' into a 'Iter' with 'Gen.iter', but note that 'Gen' is
  // much more powerful than 'Iter' but at the cost of being more slow due to
  // overhead from monadic structure.
  //
  // The most powerful implementation of a stream is
  // 'https://hackage.haskell.org/package/streaming', but 'Gen' is meant to only
  // be a lightweight alternative to a full-blown streaming library.
  //
  // While 'Gen' is an inferior implementation of
  // 'https://hackage.haskell.org/package/streaming', it still has a lot of very
  // powerful combinators that you can use.

  [class-decl Gen [V *] [A *]]
  [class-struct Gen
    [field on_run [-> , [Either A [; V [Gen V A]]]]]
  ]

  [def-decl pub Gen.uncons [-> [type V] [type A] [Gen V A] [Either A [; V [Gen V A]]]]]
  [def-impl Gen.uncons [\ [type V] [type A] self
    [Gen.on_run self ,]
  ]]

  [def-decl pub Gen.then [-> [type A] [type B] [type V] [Gen V A] [-> A [Gen V B]] [Gen V B]]]
  [def-impl Gen.then [\ [type A] [type B] [type V] self a->m.b
    [Gen.new [\ _ [do
      [match [Gen.uncons self]
        [[L a] [Gen.uncons [a->m.b a]]]
        [[R [; v next]] [Right [; v [Gen.then next a->m.b]]]]
      ]
    ]]]
  ]]

  [def-decl pub Gen.pure [-> [type A] [type V] A [Gen V A]]]
  [def-impl Gen.pure [\ [type A] [type V] a [Gen.new [\ _ [Left a]]]]]

  [def-decl pub Gen.map [-> [type A] [type B] [type V] [Gen V A] [-> A B] [Gen V B]]]
  [def-impl Gen.map [\ [type A] [type B] [type V] self a->b
    [self . Gen.then [\ a [Gen.pure [a->b a]]]]
  ]]

  [def-decl pub Gen.<*> [-> [type A] [type B] [type V] [Gen V [-> A B]] [Gen V A] [Gen V B]]]
  [def-impl Gen.<*> [\ [type A] [type B] [type V] mf ma [
    [mf . Gen.then [\ f [ma . Gen.then [\ a [Gen.pure [f a]]]]]]
  ]]]

  [def-decl pub auto Gen.Functor [-> [type V] [Functor [Gen V]]]]
  [def-impl Gen.Functor [\ [type V] [Functor.new [\ [type A] [type B] self a->b [self . Gen.map a->b]]]]]

  [def-decl pub auto Gen.Apply [-> [type V] [Apply [Gen V]]]]
  [def-impl Gen.Apply [\ [type V] [Apply.new [\ [type A] [type B] mf ma [mf . Gen.<*> ma]]]]]

  [def-decl pub auto Gen.Pure [-> [type V] [Pure [Gen V]]]]
  [def-impl Gen.Pure [\ [type V] [Pure.new [\ [type A] a [Gen.pure a]]]]]

  [def-decl pub auto Gen.Then [-> [type V] [Then [Gen V]]]]
  [def-impl Gen.Then [\ [type V] [Then.new [\ [type A] [type B] ma a->mb [Gen.then ma a->mb]]]]]

  [def-decl pub Gen.yield [-> [type V] V [Gen V ,]]]
  [def-impl Gen.yield [\ [type V] value
    [Gen.new [\ _ [Right [; value [Gen.pure ,]]]]]
  ]]

  // Wrap an impure function as 'Gen'. This is so that impure actions (usually
  // just those before the first 'Gen.yield') do not get executed until the
  // generator is actually run.
  [def-decl pub Gen.lazy [-> [type V] [type A] [-> , [Gen V A]] [Gen V A]]]
  [def-impl Gen.lazy [\ [type V] [type A] m [do
    [Gen.new [\ _ [Gen.on_run [m ,] ,]]]
  ]]]

  [def-decl pub Gen.iter [-> [type V] [Gen V ,] [Iter V]]]
  [def-impl Gen.iter [\ [type V] in_self [do
    [let self [var in_self]]
    [Iter.new [\ _ [do
      [match [Gen.uncons [get self]]
        [[L _] [None]]
        [[R [; v next]] [do
          [set self next]
          [Some v]
        ]]
      ]
    ]]]
  ]]]

  // Like Python's 'zip()'
  [def-decl pub Gen.zip [-> [type R] [type A] [type B] [Gen A R] [Gen B R] [Gen [; A B] R]]]
  [def-impl Gen.zip [\ [type R] [type A] [type B] in_xs in_ys
    [Gen.new [\ _ [do
      [let= [R [; x xs]] [Gen.uncons in_xs]
        [[L r] [Left r]]]
      [let= [R [; y ys]] [Gen.uncons in_ys]
        [[L r] [Left r]]]
      [let z [; x y]]
      [let zs [Gen.zip xs ys]]
      [Right [; z zs]]
    ]]]
  ]]

  // * Base library: Range
  //
  // Inspired by Python's 'range()'.

  [class-decl Range [A *]]
  [class-struct Range
    [field start A]
    [field stop A]
    [field step A]
  ]

  [def-decl pub Range.length [-> [Range I32] I32]]
  [def-impl Range.length [\ self [do
    [let= [mk [; start stop step]] self]
    [I32.max 0 [[stop . I32.- start] . I32.floordiv step]]
  ]]]

  // Like Python's 'range(n)' (end-exclusive)
  [def-decl pub Range.to [-> I32 [Range I32]]]
  [def-impl Range.to [\ n [Range.new 0 n 1]]]

  // Like Python's 'range(n+1)' (end-inclusive)
  [def-decl pub Range.to= [-> I32 [Range I32]]]
  [def-impl Range.to= [\ n [Range.new 0 [I32.suc n] 1]]]

  // Shift the whole range by some offset. For example, 'Range.offset
  // [Range.from_to 0 5] 10' produces a range that generates the same sequence
  // of numbers as 'Range.from_to 10 15'.
  [def-decl pub Range.shift [-> [Range I32] I32 [Range I32]]]
  [def-impl Range.shift [\ self offset [do
    [let= [mk [; start stop step]] self]
    [Range.new [start . I32.+ offset] [stop . I32.+ offset] step]
  ]]]

  // Like Python's 'range(start, stop, 1)' (end-exclusive)
  [def-decl pub Range.from_to [-> I32 I32 [Range I32]]]
  [def-impl Range.from_to [\ start stop [Range.new start stop 1]]]

  // Like Python's 'range(start, stop+1, 1)' (end-inclusive)
  [def-decl pub Range.from_to= [-> I32 I32 [Range I32]]]
  [def-impl Range.from_to= [\ start stop [Range.new start [I32.suc stop] 1]]]

  [def-decl pub Range.is_empty [-> [Range I32] Bool]]
  [def-impl Range.is_empty [\ self [self . Range.length . == 0]]]

  // Returns the last number in the range.
  [def-decl pub Range.last [-> [Range I32] I32]]
  [def-impl Range.last [\ self [do
    [let len [Range.length self]]
    [if [len . == 0]
      [panic "Range.last: got empty range, there is no last element"]
      [Range.start self . I32.+ [Range.step self . I32.* [Range.length self . I32.- 1]]]
    ]
  ]]]

  // Defined to return a range that produces the same sequence of numbers but in
  // reverse order. No guarantee on the exact start/stop/step values of the
  // output.
  //
  // Has the exact same implementation as Python's range().__reversed__()
  // method. See
  // https://github.com/python/cpython/blob/1dfe99ae3bed6cac01732b47bf7fa637abadf51b/Objects/rangeobject.c#L1343
  [def-decl pub Range.reverse [-> [Range I32] [Range I32]]]
  [def-impl Range.reverse [\ self [do
    // Reference:
    // https://github.com/python/cpython/blob/1dfe99ae3bed6cac01732b47bf7fa637abadf51b/Objects/rangeobject.c#L1343
    [let len [Range.length self]]
    [let= [mk [; start stop step]] self]
    [let new_stop [I32.- start step]]
    [let new_start [I32.+ new_stop [I32.* len step]]]
    [Range.new new_start new_stop [I32.negate step]]
  ]]]

  [def-decl pub Range.mk_should_continue_checker [-> [Range I32] [-> I32 Bool]]]
  [def-impl Range.mk_should_continue_checker [\ self [do
    [let= [mk [; start stop step]] self]
    [if [step . > 0]
      [\ i [i . < stop]]
      [\ i [i . > stop]]
    ]
  ]]]

  [def-decl pub Range.iter [-> [Range I32] [Iter I32]]]
  [def-impl Range.iter [\ self [do
    [let i [var [self . Range.start]]]
    [let range_should_continue [self . Range.mk_should_continue_checker]]
    [Iter.new [\ _ [do
      [if [range_should_continue [get i]]
        [do
          [let curr [get i]]
          [set i [curr . I32.+ [self . Range.step]]]
          [Some curr]
        ]
        [None]
      ]
    ]]]
  ]]]

  [def-decl pub Range.gen [-> [Range I32] [Gen I32 ,]]]
  [def-impl Range.gen [\ self [do
    [let= [mk [; start stop step]] self]
    [let range_should_continue [Range.mk_should_continue_checker self]]

    [let*
      [go [\ [i I32] [:: [type [Gen I32 ,]] [do
        [if [range_should_continue i]
          [do
            [try _ [Gen.yield i]]
            [go [i . I32.+ step]]
          ]
          [pure ,]
        ]
      ]]]]
    ]

    [go start]
  ]]]

  [def-decl pub Range.foreach [-> [Range I32] [-> I32 ShouldContinue] ,]]
  [def-impl Range.foreach [\ self callback [do
    [let= [mk [; start stop step]] self]
    [let range_should_continue [Range.mk_should_continue_checker self]]
    [let*
      [go [\ i [
        [if [range_should_continue i]
          [do
            [let should_continue [callback i]]
            [if should_continue [go [i . I32.+ step]] ,]
          ]
          ,
        ]
      ]]]
    ]
    [go start]
  ]]]

  [def-decl pub Range.traverse [-> [type M] [Range I32] [-> I32 [M ShouldContinue]] [auto [Monad M]] [M ,]]]
  [def-impl Range.traverse [\ [type M] self callback [auto M.Monad] [do
    [let= [mk [; start stop step]] self]
    [let range_should_continue [Range.mk_should_continue_checker self]]
    [let*
      [go [\ [i I32] [:: [type [M ,]] [
        [if [range_should_continue i]
          [do
            [try should_continue [callback i]]
            [if should_continue [go [i . I32.+ step]] [pure [type M] ,]]
          ]
          [pure ,]
        ]
      ]]]]
    ]
    [go start]
  ]]]

  // Equivalent to 'for (i = 0; i < n; i<>) { if (!callback(i)) break, }' in
  // other languages.
  [def-decl pub loop<n [-> I32 [-> I32 ShouldContinue] ,]]
  [def-impl loop<n [\ n callback [do
    [Range.to n . Range.foreach callback]
  ]]]

  // Equivalent to 'for (i = 0; i <= n; i<>) { if (!callback(i)) break, }' in
  // other languages.
  [def-decl pub loop<=n [-> I32 [-> I32 ShouldContinue] ,]]
  [def-impl loop<=n [\ n callback [do
    [Range.to= n . Range.foreach callback]
  ]]]

  // Equivalent to 'for (i = n-1; i >= 0; i--) { if (!callback(i)) break, }' in
  // other languages
  [def-decl pub loop<n_reversed [-> I32 [-> I32 ShouldContinue] ,]]
  [def-impl loop<n_reversed [\ n callback [do
    [Range.to n . Range.reverse . Range.foreach callback]
  ]]]

  // Equivalent to 'for (i = n; i >= 0; i--) { if (!callback(i)) break, }' in
  // other languages, but in reverse order.
  [def-decl pub loop<=n_reversed [-> I32 [-> I32 ShouldContinue] ,]]
  [def-impl loop<=n_reversed [\ n callback [do
    [Range.to= n . Range.reverse . Range.foreach callback]
  ]]]

  // Repeatedly apply a function N times. If less than or equal to 0, does
  // nothing and returns the input.
  [def-decl pub fn.repeat [-> [type A] I32 [-> A A] A A]]
  [def-impl fn.repeat [\ [type A] n_times func x [do
    [let x [var x]]
    [loop<n n_times [\ _ [do
      [update x func]
      continue
    ]]]
    [get x]
  ]]]

  // * Base library: HostEither
  //
  // An opaque 'Either' type from the host language.
  //
  // The main use case is to convert it to our own 'Either' type via
  // 'HostEither.to_either' and use it.
  //
  // The reason this exists is because sometimes it is easier for the host
  // platform to a concrete 'HostEither' type instead of our own 'Either' type
  // (which is sometimes finicky to construct from the host platform).

  [class-decl HostEither [L *] [R *]]

  [def-decl pub HostEither.is_left [-> [type L] [type R] [HostEither L R] Bool]]
  [def-annotate HostEither.is_left "foreign-import:host_either_is_left"]

  [def-decl pub HostEither.is_right [-> [type L] [type R] [HostEither L R] Bool]]
  [def-annotate HostEither.is_right "foreign-import:host_either_is_right"]

  [def-decl pub HostEither.unwrap_left [-> [type L] [type R] [HostEither L R] L]]
  [def-annotate HostEither.unwrap_left "foreign-import:host_either_unwrap_left"]

  [def-decl pub HostEither.unwrap_right [-> [type L] [type R] [HostEither L R] R]]
  [def-annotate HostEither.unwrap_right "foreign-import:host_either_unwrap_right"]

  [def-decl pub HostEither.to_either [-> [type L] [type R] [HostEither L R] [Either L R]]]
  [def-impl HostEither.to_either [\ [type L] [type R] self
    [if [self . HostEither.is_left]
      [Left [self . HostEither.unwrap_left]]
      [Right [self . HostEither.unwrap_right]]
    ]
  ]]

  // * Base library: HostIterator
  //
  // An opaque iterator from the host language (e.g. JavaScript) that we can
  // wrap and use in our code.
  //
  // The main use case is to convert it to our own Iterator type via
  // 'HostIterator.to_iterator' and use it.

  [class-decl HostIterator [A *]]

    [def-decl pub HostIterator.has_next [-> [type A] [HostIterator A] Bool]]
    [def-annotate HostIterator.has_next "foreign-import:host_iterator_has_next"]

    // Panics if there's no next item.
    [def-decl pub HostIterator.next [-> [type A] [HostIterator A] A]]
    [def-annotate HostIterator.next "foreign-import:host_iterator_next"]

  [def-decl pub HostIterator.to_iter [-> [type A *] [HostIterator A] [Iter A]]]
  [def-impl HostIterator.to_iter [\ [type A *] self
    [Iter.new [\ _ [do
      [if [self . HostIterator.has_next]
        [Some [self . HostIterator.next]]
        [None]
      ]
    ]]]
  ]]

  // * Base library: Vec. A known type in DSL and has that '[vec ...]' syntax.
  //
  // A [Vec A] is an immutable  of data with guaranteed O(1) .length(),
  // .slice(), and .at(). List is intented only for users to write a List
  // literals. Other functionalities likely have O(N) time.
  //
  // Should behave like a C array with bare-metal random access and slicing
  // (e.g., via pointer arithmetic). performance, and similarly difficult to
  // manipulate (e.g., no push, pop, concat, etc.).

  [def-decl pub Vec.length [-> [type A *] [Vec A] I32]]
  [def-annotate Vec.length "foreign-import:vec_length"]

  [def-decl pub Vec.at [-> [type A *] [Vec A] I32 A]]
  [def-annotate Vec.at "foreign-import:vec_at"]

  [def-decl pub Vec.foreach [-> [type A *] [Vec A] [-> A ShouldContinue] ,]]
  [def-impl Vec.foreach [\ [type A *] self callback
    [loop<n [self . Vec.length] [\ i
      [callback [self . Vec.at i]]
    ]]
  ]]

  [def-decl pub Vec.foreach_reversed [-> [type A *] [Vec A] [-> A ShouldContinue] ,]]
  [def-impl Vec.foreach_reversed [\ [type A *] self callback [do
    [loop<n_reversed [self . Vec.length] [\ i
      [callback [self . Vec.at i]]
    ]]
  ]]]

  [def-decl pub Vec.foldl [-> [type A *] [type S *] [Vec A] S [-> S A S] S]]
  [def-impl Vec.foldl [\ [type A *] [type S *] self init callback [do
    [let state [var init]]
    [self . Vec.foreach [\ a [do
      [set state [callback [get state] a]]
      continue
    ]]]
    [get state]
  ]]]

  [def-decl pub Vec.foldr [-> [type A *] [type S *] [Vec A] S [-> A S S] S]]
  [def-impl Vec.foldr [\ [type A *] [type S *] self init callback [do
    [let state [var init]]
    [self . Vec.foreach_reversed [\ a [do
      [set state [callback a [get state]]]
      continue
    ]]]
    [get state]
  ]]]

  [def-decl pub auto Vec.Foldable [Foldable Vec]]
  [def-impl Vec.Foldable [Foldable.new Vec.foldl]]

  [def-decl pub Vec.iter_on_range [-> [type A] [Vec A] [-> [Range I32] [Range I32]] [Iter A]]]
  [def-impl Vec.iter_on_range [\ [type A] self mod_range
    [Range.to [self . Vec.length]
      . mod_range
      . Range.iter
      . Iter.map [\ i [self . Vec.at i]]]
  ]]

  [def-decl pub Vec.iter [-> [type A] [Vec A] [Iter A]]]
  [def-impl Vec.iter [\ [type A] self [self . Vec.iter_on_range [fn.id]]]]

  // * Base library: Immutable Seq
  //
  // Has guaranteed O(1) .length(); amortized O(1) .cons, .snoc, etc.; Amortized
  // logarithmic time .at() and .concat().
  //
  // Should behave like Haskell's Seq.

  // TODO: Use a finger tree implementation.
  [class-decl Seq [A *]]

  [def-decl pub Seq.nil [-> [type A] [Seq A]]]
  [def-annotate Seq.nil "foreign-import:seq_nil"]

  [def-decl pub Seq.length [-> [type A] [Seq A] I32]]
  [def-annotate Seq.length "foreign-import:seq_length"]

  [def-decl pub Seq.is_empty [-> [type A] [Seq A] Bool]]
  [def-annotate Seq.is_empty "foreign-import:seq_is_empty"]

  [def-decl pub Seq.cons [-> [type A] [Seq A] A [Seq A]]]
  [def-annotate Seq.cons "foreign-import:seq_cons"]

  [def-decl pub Seq.snoc [-> [type A] [Seq A] A [Seq A]]]
  [def-annotate Seq.snoc "foreign-import:seq_snoc"]

  [def-decl pub Seq.reverse [-> [type A] [Seq A] [Seq A]]]
  [def-annotate Seq.reverse "foreign-import:seq_reverse"]

  [def-decl pub Seq.at [-> [type A] [Seq A] I32 A]]
  [def-annotate Seq.at "foreign-import:seq_at"]

  [def-decl pub Seq.at? [-> [type A] [Seq A] I32 [Maybe A]]]
  [def-impl Seq.at? [\ [type A] self i
    [if [Bool.and [0 . <= i] [i . < [self . Seq.length]]]
      [Some [self . Seq.at i]]
      [None]
    ]
  ]]

  [def-decl pub Seq.one [-> [type A] A [Seq A]]]
  [def-impl Seq.one [\ [type A] a [Seq.nil . Seq.cons a]]]

  [def-decl pub Seq.<> [-> [type A] [Seq A] [Seq A] [Seq A]]]
  [def-annotate Seq.<> "foreign-import:seq_concat"]

  [def-decl pub auto Seq.Semigroup [-> [type A] [Semigroup [Seq A]]]]
  [def-impl Seq.Semigroup [\ [type A] [Semigroup.new [Seq.<>]]]]

  [def-decl Seq.create_host_iterator [-> [type A] [Seq A] [HostIterator A]]]
  [def-annotate Seq.create_host_iterator "foreign-import:seq_create_host_iterator"]

  [def-decl Seq.create_reversed_host_iterator [-> [type A] [Seq A] [HostIterator A]]]
  [def-annotate Seq.create_reversed_host_iterator "foreign-import:seq_create_reversed_host_iterator"]

  // Iterates through the 'Seq' in O(N) time. (NOT O(N x log N) time, which is
  // what you'd get if you naively implemented it via 'Seq.at()' and
  // 'Range.iter()').
  [def-decl pub Seq.iter [-> [type A] [Seq A] [Iter A]]]
  [def-impl Seq.iter [\ [type A] self [do
    [self . Seq.create_host_iterator . HostIterator.to_iter]
  ]]]

  // O(N). Faster than 'Seq.reverse' then 'Seq.iter'
  [def-decl pub Seq.iter_reversed [-> [type A] [Seq A] [Iter A]]]
  [def-impl Seq.iter_reversed [\ [type A] self [do
    [self . Seq.create_reversed_host_iterator . HostIterator.to_iter]
  ]]]

  [def-decl pub Seq.take [-> [type A] [Seq A] I32 [Seq A]]]
  [def-annotate Seq.take "foreign-import:seq_take"]

  [def-decl pub Seq.drop [-> [type A] [Seq A] I32 [Seq A]]]
  [def-annotate Seq.drop "foreign-import:seq_drop"]

  [def-decl pub Seq.take_and_drop [-> [type A] [Seq A] I32 [; [Seq A] [Seq A]]]]
  [def-annotate Seq.take_and_drop "foreign-import:seq_take_and_drop"]

  [def-decl pub Seq.take_end [-> [type A] [Seq A] I32 [Seq A]]]
  [def-impl Seq.take_end [\ [type A] self len [
    [self . Seq.drop [[self . Seq.length . I32.- len]]]
  ]]]

  [def-decl pub Seq.drop_end [-> [type A] [Seq A] I32 [Seq A]]]
  [def-impl Seq.drop_end [\ [type A] self len [
    [self . Seq.take [self . Seq.length . I32.- len]]
  ]]]

  // NOTE: It is possible to have a fast O(N) implementation of
  // https://hackage-content.haskell.org/package/containers/docs/src/Data.Sequence.Internal.html#inits
  [def-decl pub Seq.inits_host_iterator [-> [type A] [Seq A] [HostIterator [Seq A]]]]
  [def-annotate Seq.inits_host_iterator "foreign-import:seq_inits_host_iterator"]

  // NOTE: It is possible to have a fast O(N) implementation of
  // https://hackage-content.haskell.org/package/containers/docs/src/Data.Sequence.Internal.html#tails
  [def-decl pub Seq.tails_host_iterator [-> [type A] [Seq A] [HostIterator [Seq A]]]]
  [def-annotate Seq.tails_host_iterator "foreign-import:seq_tails_host_iterator"]

  // If backend has an efficient implementation, O(N).
  [def-decl pub Seq.inits [-> [type A] [Seq A] [Iter [Seq A]]]]
  [def-impl Seq.inits [\ [type A] self
    [self . Seq.inits_host_iterator . HostIterator.to_iter]
  ]]

  // If backend has an efficient implementation, O(N).
  [def-decl pub Seq.tails [-> [type A] [Seq A] [Iter [Seq A]]]]
  [def-impl Seq.tails [\ [type A] self
    [self . Seq.tails_host_iterator . HostIterator.to_iter]
  ]]

  [def-decl pub Seq.uncons [-> [type A] [Seq A] [Maybe [; A [Seq A]]]]]
  [def-impl Seq.uncons [\ [type A] self [do
    // TODO: Optimize
    [if [self . Seq.is_empty]
      [None]
      [Some [; [self . Seq.at 0] [self . Seq.drop 1]]]
    ]
  ]]]

  // * Base library: (Mutable) Array.
  //
  // Like 'Vec' but has amortized O(1) .append(), etc, though has bad .prepend()
  // performance.
  //
  // Behaves like a typical C<> std::vector / a JavaScript Array / a Java
  // ArrayList / a Python list.

  [class-decl Array [A *]]

  [def-decl pub Array.create [-> [type A] , [Array A]]]
  [def-annotate Array.create "foreign-import:array_create"]

  [def-decl pub Array.is_empty [-> [type A] [Array A] Bool]]
  [def-annotate Array.is_empty "foreign-import:array_is_empty"]

  [def-decl pub Array.clear [-> [type A] [Array A] ,]]
  [def-annotate Array.clear "foreign-import:array_clear"]

  [def-decl pub Array.length [-> [type A] [Array A] I32]]
  [def-annotate Array.length "foreign-import:array_length"]

  [def-decl pub Array.set [-> [type A] [Array A] I32 A ,]]
  [def-annotate Array.set "foreign-import:array_set"]

  [def-decl pub Array.at [-> [type A] [Array A] I32 A]]
  [def-annotate Array.at "foreign-import:array_at"]

  [def-decl pub Array.at? [-> [type A] [Array A] I32 [Maybe A]]]
  [def-impl Array.at? [\ [type A] arr i
    [if [Bool.and [0 . <= i] [i . < [arr . Array.length]]]
      [Some [arr . Array.at i]]
      [None]
    ]
  ]]

  [def-decl pub Array.update [-> [type A] [Array A] I32 [-> A A] ,]]
  [def-impl Array.update [\ [type A] arr i f [do
    [let old_value [arr . Array.at i]]
    [let new_value [f old_value]]
    [arr . Array.set i new_value]
  ]]]

  [def-decl pub Array.is_last_ix [-> [type A] [Array A] I32 Bool]]
  [def-impl Array.is_last_ix [\ [type A] arr i
    [i . == [arr . Array.length . I32.- 1]]
  ]]

  [def-decl pub Array.append [-> [type A] [Array A] A ,]]
  [def-annotate Array.append "foreign-import:array_append"]

  [def-decl pub Array.prepend [-> [type A] [Array A] A ,]]
  [def-annotate Array.prepend "foreign-import:array_prepend"]

  [def-decl pub Array.delete [-> [type A] [Array A] I32 ,]]
  [def-annotate Array.delete "foreign-import:array_delete"]

  [def-decl pub Array.is_one [-> [type A] [Array A] Bool]]
  [def-impl Array.is_one [\ [type A] arr
    [arr . Array.length . == 1]
  ]]

  [def-decl pub Array.one [-> [type A] A [Array A]]]
  [def-impl Array.one [\ [type A] x1 [do
    [let arr [Array.create ,]]
    [arr . Array.append x1]
    arr
  ]]]

  [def-decl pub Array.two [-> [type A] A A [Array A]]]
  [def-impl Array.two [\ [type A] x1 x2 [do
    [let arr [Array.create ,]]
    [arr . Array.append x1]
    [arr . Array.append x2]
    arr
  ]]]

  [def-decl pub Array.three [-> [type A] A A A [Array A]]]
  [def-impl Array.three [\ [type A] x1 x2 x3 [do
    [let arr [Array.create ,]]
    [arr . Array.append x1]
    [arr . Array.append x2]
    [arr . Array.append x3]
    arr
  ]]]

  [def-decl pub Array.pop? [-> [type A] [Array A] I32 [Maybe A]]]
  [def-impl Array.pop? [\ [type A] arr i [do
    [try item [Array.at? arr i]]
    [Array.delete arr i]
    [pure item]
  ]]]

  [def-decl pub Array.pop [-> [type A] [Array A] I32 A]]
  [def-impl Array.pop [\ [type A] arr i [do
    [arr . Array.pop? i . Maybe.expect* [\ _
      [panic "Array.pop: index ("
        . String.<> [i . I32.to_str]
        . String.<> ") out of bounds (Array length: "
        . String.<> [arr . Array.length . I32.to_str]
        . String.<> ")"]
    ]]
  ]]]

  [def-decl pub Array.pop_first? [-> [type A] [Array A] [Maybe A]]]
  [def-impl Array.pop_first? [\ [type A] arr [do
    [arr . Array.pop? 0]
  ]]]

  [def-decl pub Array.pop_first [-> [type A] [Array A] A]]
  [def-impl Array.pop_first [\ [type A] arr [do
    [arr . Array.pop_first? . Maybe.unwrap]
  ]]]

  [def-decl pub Array.pop_last? [-> [type A] [Array A] [Maybe A]]]
  [def-impl Array.pop_last? [\ [type A] arr [do
    [arr . Array.pop? [arr . Array.length . I32.- 1]]
  ]]]

  [def-decl pub Array.pop_last [-> [type A] [Array A] A]]
  [def-impl Array.pop_last [\ [type A] arr [do
    [arr . Array.pop_last? . Maybe.unwrap]
  ]]]

  [def-decl pub Array.from_foldable [-> [type F] [type A] [F A] [auto [Foldable F]] [Array A]]]
  [def-impl Array.from_foldable [\ [type F] [type A] f.a [auto F.Foldable] [do
    [let arr [Array.create ,]]
    [f.a . Foldable.foreach [\ a [arr . Array.append a]]]
    arr
  ]]]

  [def-decl pub Array.foreach [-> [type A *] [Array A] [-> A ShouldContinue] ,]]
  [def-impl Array.foreach [\ [type A *] self callback
    [loop<n [self . Array.length] [\ i
      [callback [self . Array.at i]]
    ]]
  ]]

  [def-decl pub Array.foreach_reversed [-> [type A *] [Array A] [-> A ShouldContinue] ,]]
  [def-impl Array.foreach_reversed [\ [type A *] self callback
    [loop<n_reversed [self . Array.length] [\ i
      [callback [self . Array.at i]]
    ]]
  ]]

  // NOTE: This function allows the Array's length to change during iteration,
  // and will check the length on every iteration. This is similar to how
  // JavaScript's Array.filter() behaves.
  [def-decl pub Array.filter_inplace [-> [type A *] [Array A] [-> A Bool] ,]]
  [def-impl Array.filter_inplace [\ [type A *] self predicate [do
    [let* [go [\ [i I32] [do
      [if [i . < [self . Array.length]]
        [if [predicate [self . Array.at i]]
          [go [i . I32.suc]]
          [do [self . Array.delete i] [go i]]
        ]
        ,
      ]
    ]]]]
    [go 0]
  ]]]

  [def-decl pub Array.map [-> [type A] [type B] [Array A] [-> A B] [Array B]]]
  [def-impl Array.map [\ [type A] [type B] self f [do
    // NOTE: The implementation should work even when the Array is mutated
    // during map. We will follow how JavaScript's Array.map() behaves.
    [let out [Array.create ,]]
    [let* [go [\ [i I32] [do
      [if [i . < [self . Array.length]]
        [do
          [out . Array.append [f [self . Array.at i]]]
          [go [i . I32.suc]]
        ]
        ,
      ]
    ]]]]
    [go 0]
    out
  ]]]

  [def-decl pub auto Array.Functor [Functor Array]]
  [def-impl Array.Functor [Functor.new [\ [type A] [type B] [Array.map]]]]

  [def-decl pub Array.reduce [-> [type A] [type S] [Array A] S [-> S A S] S]]
  [def-impl Array.reduce [\ [type A] [type S] self state step [do
    // NOTE: The implementation should work even when the Array is mutated
    // during map. We will follow how JavaScript's Array.map() behaves.
    [let* [go [\ [i I32] [state S] [do
      [if [i . < [self . Array.length]]
        [go [i . I32.suc] [step state [self . Array.at i]]]
        state
      ]
    ]]]]
    [go 0 state]
  ]]]

  [def-decl pub auto Array.Foldable [Foldable Array]]
  [def-impl Array.Foldable [Foldable.new [\ [type A] [type S] [Array.reduce]]]]

  [def-decl pub Array.find_some [-> [type A] [type B] [Array A] [-> A [Maybe B]] [Maybe B]]]
  [def-impl Array.find_some [\ [type A] [type B] self predicate [do
    [let result [var [None]]]
    [self . Array.foreach [\ a [do
      [match [predicate a]
        [[None] continue]
        [[Some b] [do
          [set result [Some b]]
          break
        ]]
      ]
    ]]]
    [get result]
  ]]]

  [def-decl pub Array.find [-> [type A] [Array A] [-> A Bool] [Maybe A]]]
  [def-impl Array.find [\ [type A] self predicate
    [self . Array.find_some [\ a [if [predicate a] [Some a] [None]]]]
  ]]

  [def-decl pub Array.traverse_ [-> [type A] [type M] [Array A] [auto [Monad M]] [-> A [M ShouldContinue]] [M ,]]]
  [def-impl Array.traverse_ [\ [type A] [type M] arr [auto M.Monad] callback [do
    [let* [go [\ [i I32] [do
      [if [i . < [arr . Array.length]]
        [do
          [try _ [callback [arr . Array.at i]]]
          [go [i . I32.suc]]
        ]
        [pure ,]
      ]
    ]]]]
    [go 0]
  ]]]

  [def-decl pub Array.traverse [-> [type A] [type B] [type M] [Array A] [auto [Monad M]] [-> A [M B]] [M [Array B]]]]
  [def-impl Array.traverse [\ [type A] [type B] [type M] self [auto M.Monad] f [do
    [let len [self . Array.length]]
    [let out [Array.create ,]]

    [let* [go [\ [i I32] [do
      [if [i . < len]
        [do
          [let a [self . Array.at i]]
          [try b [f a]]
          [out . Array.append b]
          [go [i . I32.suc]]
        ]
        [pure out]
      ]
    ]]]]
    [go 0]
  ]]]

  [def-decl pub Array.from_iter [-> [type A] [Iter A] [Array A]]]
  [def-impl Array.from_iter [\ [type A] self [do
    [let arr [Array.create ,]]
    [self . Iter.foreach [\ a [do
      [arr . Array.append a]
      continue
    ]]]
    arr
  ]]]

  [def-decl pub Array.iter_on_range [-> [type A] [Array A] [-> [Range I32] [Range I32]] [Iter A]]]
  [def-impl Array.iter_on_range [\ [type A] self range_mod [do
    [Range.to [self . Array.length]
      . range_mod
      . Range.iter
      . Iter.map [\ i [self . Array.at i]]
    ]
  ]]]

  [def-decl pub Array.iter_reversed [-> [type A] [Array A] [Iter A]]]
  [def-impl Array.iter_reversed [\ [type A] self [do
    [Array.iter_on_range self [Range.reverse]]
  ]]]

  [def-decl pub Array.iter [-> [type A] [Array A] [Iter A]]]
  [def-impl Array.iter [\ [type A] self [do
    [self . Array.iter_on_range [fn.id]]
  ]]]

  [def-decl pub Array.swap [-> [type A] [Array A] I32 I32 ,]]
  [def-impl Array.swap [\ [type A] self i j [do
    [let temp [self . Array.at i]]
    [self . Array.set i [self . Array.at j]]
    [self . Array.set j temp]
  ]]]

  [def-decl pub Array.create_filled [-> [type A] I32 A [Array A]]]
  [def-impl Array.create_filled [\ [type A] len value [do
    [let arr [Array.create ,]]
    [loop<n len [\ _ [do
      [arr . Array.append value]
      continue
    ]]]
    arr
  ]]]

  [def-decl pub Array.create_filled* [-> [type A] I32 [-> , A] [Array A]]]
  [def-impl Array.create_filled* [\ [type A] len fill_fn [do
    [let arr [Array.create ,]]
    [loop<n len [\ _ [do
      [arr . Array.append [fill_fn ,]]
      continue
    ]]]
    arr
  ]]]

  [def-decl pub Array.map_inplace [-> [type A] [Array A] [-> A A] ,]]
  [def-impl Array.map_inplace [\ [type A] self f [do
    [loop<n [self . Array.length] [\ i [do
      [self . Array.update i f]
      continue
    ]]]
  ]]]

  // Creates a completely new Array with the specified slice of the original
  // Array.
  [def-decl pub Array.sliced [-> [type A] [Array A] I32 I32 [Array A]]]
  [def-impl Array.sliced [\ [type A] self start stop [do
    [if-let [true] [start . < 0] [do
      [panic ["Array.sliced: start index ("
        . String.<> [I32.to_str start]
        . String.<> ") cannot be negative"]]
    ]]

    [if-let [true] [stop . > [self . Array.length]] [do
      [panic ["Array.sliced: stop index ("
        . String.<> [I32.to_str stop]
        . String.<> ") cannot be out of bounds (array length: "
        . String.<> [[self . Array.length] . I32.to_str]
        . String.<> ")"]]
    ]]

    [let output [Array.create ,]]
    [Range.from_to start stop . Range.foreach [\ i [do
      [output . Array.append [self . Array.at i]]
      continue
    ]]]
    output
  ]]]

  // Equivalent to 'Array.slice', but drops the first 'n_dropped' elements.
  // Over-dropping (i.e., when 'n_dropped' is greater than the Array's length)
  // returns an empty Array.
  [def-decl pub Array.dropped [-> [type A] [Array A] I32 [Array A]]]
  [def-impl Array.dropped [\ [type A] self n_dropped [do
    [let len [self . Array.length]]
    [let start [Ord.min n_dropped len]]
    [Array.sliced self start len]
  ]]]

  [def-decl pub Array.reverse_inplace [-> [type A] [Array A] ,]]
  [def-impl Array.reverse_inplace [\ [type A] arr [do
    [let len [arr . Array.length]]
    [let len_minus_1 [len . I32.- 1]]
    [loop<n [len . I32.floordiv 2] [\ l [do
      [let r [I32.- len_minus_1 l]]
      [arr . Array.swap l r]
      continue
    ]]]
  ]]]

  [def-decl pub Array.sort_inplace_by [-> [type A] [Array A] [-> A A Ordering] ,]]
  [def-impl Array.sort_inplace_by [\ [type A] self compare [do
    // TODO: Optimize this. Current implementation is an unstable quick sort
    // with a simple pivot.
    //
    // FIXME: THIS IS BUGGED. FIX LATER

    [if-let [true] [self . Array.is_empty] ,]

    [let* [go [\ [left I32] [right I32] [do
      // NOTE: Right index is inclusive
      [if-let [false] [left . < right] ,]

      [let pivot_ix left]
      [let pivot [self . Array.at pivot_ix]]

      [let l [var [left . I32.suc]]]
      [let r [var right]]

      [loop [\ _ [do
        [loop [\ _ [do
          [if-let [false] [compare [self . Array.at [get l]] pivot . Ordering.is_<] break]
          [update l I32.suc]
          continue
        ]]]

        [loop [\ _ [do
          [if-let [false] [< [get l] [get r]] break]
          [if-let [false] [compare [self . Array.at [get r]] pivot . Ordering.is_>] break]
          [update r I32.dec]
          continue
        ]]]

        [if-let [true] [== [get l] [get r]] break]

        [if [compare [self . Array.at [get l]] [self . Array.at [get r]] . Ordering.is_<]
          ,
          [do
            [Array.swap self [get l] [get r]]
          ]
        ]

        continue
      ]]]

      [self . Array.swap pivot_ix [get l]]

      [go left [get l . I32.dec]]
      [go [get l . I32.suc] right]
    ]]]]

    [go 0 [self . Array.length . I32.dec]]
  ]]]

  [def-decl pub Array.sort_inplace_on [-> [type A] [type K] [Array A] [-> A K] [auto [Ord K]] ,]]
  [def-impl Array.sort_inplace_on [\ [type A] [type K] self key_fn [auto K.Ord] [do
    [self . Array.sort_inplace_by [\ a b
      [Ord.compare [key_fn a] [key_fn b]]
    ]]
  ]]]

  // * Base library: More Seq & Vec & Array functions

  [def-decl pub Array.from_vec [-> [type A] [Vec A] [Array A]]]
  [def-annotate Array.from_vec "foreign-import:array_from_vec"]

  [def-decl pub Array.to_seq [-> [type A] [Array A] [Seq A]]]
  [def-annotate Array.to_seq "foreign-import:array_to_seq"]

  [def-decl pub Array.to_vec [-> [type A] [Array A] [Vec A]]]
  [def-annotate Array.to_vec "foreign-import:array_to_vec"]

  [def-decl pub Seq.from_vec [-> [type A] [Vec A] [Seq A]]]
  [def-annotate Seq.from_vec "foreign-import:seq_from_vec"]

  [def-decl pub Seq.to_array [-> [type A] [Seq A] [Array A]]]
  [def-annotate Seq.to_array "foreign-import:seq_to_array"]

  [def-decl pub Seq.to_vec [-> [type A] [Seq A] [Vec A]]]
  [def-annotate Seq.to_vec "foreign-import:seq_to_vec"]

  [def-decl pub Seq.from_array [-> [type A] [Array A] [Seq A]]]
  [def-impl Seq.from_array [\ [type A] [Array.to_seq]]]

  [def-decl pub Array.from_seq [-> [type A] [Seq A] [Array A]]]
  [def-impl Array.from_seq [\ [type A] [Seq.to_array]]]

  // NOTE: Runs in O(N) time, NOT O(N*log(N)).
  [def-decl pub Seq.foreach [-> [type A *] [Seq A] [-> A ShouldContinue] ,]]
  [def-impl Seq.foreach [\ [type A *] self f [do
    // TODO: Optimize
    [self . Seq.iter . Iter.foreach f]
  ]]]

  [def-decl pub Seq.map [-> [type A *] [type B *] [Seq A] [-> A B] [Seq B]]]
  [def-impl Seq.map [\ [type A *] [type B *] seq f [do
    // TODO: Optimize
    [let arr [Array.create ,]]
    [seq . Seq.foreach [\ a [do
      [arr . Array.append [f a]]
      continue
    ]]]
    [Array.to_seq arr]
  ]]]

  [def-decl pub Seq.traverse [
    -> [type A] [type B] [type M]
    [Seq A] [-> A [M B]] [auto [Monad M]] [M [Seq B]]
  ]]
  [def-impl Seq.traverse [\ [type A] [type B] [type M] seq f [auto M.Monad] [do
    // TODO: Optimize
    [try arr [seq . Seq.to_array . Array.traverse f]]
    [pure [Array.to_seq arr]]
  ]]]

  [def-decl pub Seq.from_iter [-> [type A] [Iter A] [Seq A]]]
  [def-impl Seq.from_iter [\ [type A] self [do
    // TODO: Optimize
    [self . Array.from_iter . Seq.from_array]
  ]]]

  // * Base library: Functional List (List)
  //
  // Behaves like Haskell's List.

  [class-decl List [A *]]
  [class-enum List 
    [member Nil ,]
    [member Cons [; A [List A]]]
  ]

  [def-decl pub List.nil [-> [type A] [List A]]]
  [def-impl List.nil [\ [type A] [List.Nil]]]

  [def-decl pub List.cons [-> [type A] [List A] A [List A]]]
  [def-impl List.cons [\ [type A] self x [List.Cons x self]]]

  [def-decl pub List.reduce [-> [type A] [type S] [List A] S [-> S A S] S]]
  [def-impl List.reduce [\ [type A] [type S] self state step
    [match self
      [[Nil] state]
      [[Cons [; x xs]] [List.reduce xs [step state x] step]]
    ]
  ]]

  [def-decl pub auto List.Foldable [Foldable List]]
  [def-impl List.Foldable [Foldable.new [\ [type A] [type S] [List.reduce]]]]

  [def-decl pub List.is_empty [-> [type A *] [List A] Bool]]
  [def-impl List.is_empty [\ [type A *] self
    [match self
      [[Nil] true]
      [[Cons] false]
    ]
  ]]

  [def-decl pub List.cons [-> [type A *] [List A] A [List A]]]
  [def-impl List.cons [\ [type A *] self x [List.Cons x self]]]

  [def-decl pub List.snoc [-> [type A *] [List A] A [List A]]]
  [def-impl List.snoc [\ [type A *] self x
    [match self
      [[Nil] [List.Cons x [List.Nil]]]
      [[Cons [; x xs]] [List.Cons x [xs . List.snoc x]]]
    ]
  ]]

  [def-decl pub List.one [-> [type A *] A [List A]]]
  [def-impl List.one [\ [type A *] x [List.Cons x [List.Nil]]]]

  [def-decl pub List.length [-> [type A *] [List A] I32]]
  [def-impl List.length [\ [type A *] self [do
    [let* [go [\ len [l [List A]]
      [match l
        [[Nil] len]
        [[Cons [; _ tail]] [go [I32.suc len] tail]]
      ]
    ]]]
    [go 0 self]
  ]]]

  [def-decl pub List.reverse_onto [-> [type A *] [List A] [List A] [List A]]]
  [def-impl List.reverse_onto [\ [type A *] self acc [do
    [match self
      [[Nil] acc]
      [[Cons [; x xs]] [List.reverse_onto self [List.Cons x acc]]]
    ]
  ]]]

  [def-decl pub List.reverse [-> [type A *] [List A] [List A]]]
  [def-impl List.reverse [\ [type A *] self [self . List.reverse_onto [List.Nil]]]]

  [def-decl pub List.+ [-> [type A *] [List A] [List A] [List A]]]
  [def-impl List.+ [\ [type A *] self other [do
    [match self
      [[Nil] other]
      [[Cons [; x xs]] [List.Cons x [xs . List.+ other]]]
    ]
  ]]]

  [def-decl pub List.head? [-> [type A *] [List A] [Maybe A]]]
  [def-impl List.head? [\ [type A *] self
    [match self
      [[Nil] [None]]
      [[Cons [; x _]] [Some x]]
    ]
  ]]

  [def-decl pub List.head [-> [type A *] [List A] A]]
  [def-impl List.head [\ [type A *] self [self . List.head? . Maybe.unwrap]]]

  [def-decl pub List.tail? [-> [type A *] [List A] [Maybe [List A]]]]
  [def-impl List.tail? [\ [type A *] self
    [match self
      [[Nil] [None]]
      [[Cons [; x xs]] [Some xs]]
    ]
  ]]

  [def-decl pub List.tail [-> [type A *] [List A] [List A]]]
  [def-impl List.tail [\ [type A *] self [self . List.tail? . Maybe.unwrap]]]

  [def-decl pub List.at? [-> [type A *] [List A] I32 [Maybe A]]]
  [def-impl List.at? [\ [type A *] self index [do
    [match self
      [[Nil] [None]]
      [[Cons [; x xs]]
        [if [index . == 0]
          [Some x]
          [[List.at? xs [index . I32.- 1]]]
        ]
      ]
    ]
  ]]]

  [def-decl pub List.at [-> [type A *] [List A] I32 A]]
  [def-impl List.at [\ [type A *] self index [self . List.at? index . Maybe.unwrap]]]

  [def-decl pub List.find_some_ixed [-> [type A *] [type B *] [List A] [-> I32 A [Maybe B]] [Maybe B]]]
  [def-impl List.find_some_ixed [\ [type A *] [type B *] self predicate [do
    [let* [go [\ [i I32] [l [List A]]
      [match l
        [[Nil] [None]]
        [[Cons [; x xs]]
          [match [predicate i x]
            [[Some y] [Some y]]
            [[None] [go [I32.suc i] xs]]
          ]
        ]
      ]
    ]]]
    [go 0 self]
  ]]]

  [def-decl pub List.find_some [-> [type A *] [type B *] [List A] [-> A [Maybe B]] [Maybe B]]]
  [def-impl List.find_some [\ [type A *] [type B *] self predicate [do
    [let* [go [\ [l [List A]]
      [match l
        [[Nil] [None]]
        [[Cons [; x xs]]
          [match [predicate x]
            [[Some y] [Some y]]
            [[None] [go xs]]
          ]
        ]
      ]
    ]]]
    [go self]
  ]]]

  [def-decl pub List.find [-> [type A *] [List A] [-> A Bool] [Maybe A]]]
  [def-impl List.find [\ [type A *] self predicate
    [self . List.find_some [\ x [if [predicate x] [Some x] [None]]]]
  ]]

  [def-decl pub List.filter [-> [type A *] [List A] [-> A Bool] [List A]]]
  [def-impl List.filter [\ [type A *] self predicate
    [match self
      [[Nil] [List.Nil]]
      [[Cons [; x xs]]
        [if [predicate x]
          [List.Cons x [List.filter xs predicate]]
          [List.filter xs predicate]
        ]
      ]
    ]
  ]]

  [def-decl pub List.map [-> [type A *] [type B *] [List A] [-> A B] [List B]]]
  [def-impl List.map [\ [type A *] [type B *] [\ self f [do
    [match self
      [[Nil] [List.Nil]]
      [[Cons [; x xs]] [List.Cons [f x] [List.map xs f]]]
    ]
  ]]]]

  [def-decl pub List.map_maybe [-> [type A *] [type B *] [List A] [-> A [Maybe B]] [List B]]]
  [def-impl List.map_maybe [\ [type A *] [type B *] self f [do
    [match self
      [[Nil] [List.Nil]]
      [[Cons [; x xs]]
        [match [f x]
          [[Some y] [List.Cons y [List.map_maybe xs f]]]
          [[None] [List.map_maybe xs f]]
        ]
      ]
    ]
  ]]]

  [def-decl pub List.intersperse [-> [type A] [List A] A [List A]]]
  [def-impl List.intersperse [\ [type A] self sep
    [match self
      [[Nil] [List.Nil]]
      [[Cons [; x xs]]
        [match xs
          [[Nil] [List.Cons x [List.Nil]]]
          [[Cons] [List.Cons x [List.Cons sep [List.intersperse xs sep]]]]
        ]
      ]
    ]
  ]]

  [def-decl pub List.take [-> [type A *] [List A] I32 [List A]]]
  [def-impl List.take [\ [type A *] self n
    [match self
      [[Nil] [List.Nil]]
      [[Cons [; x xs]]
        [if [n . > 0]
          [List.Cons x [List.take xs [n . I32.- 1]]]
          [List.Nil]
        ]
      ]
    ]
  ]]

  [def-decl pub List.drop [-> [type A *] [List A] I32 [List A]]]
  [def-impl List.drop [\ [type A *] self n
    [match self
      [[Nil] [List.Nil]]
      [[Cons [; x xs]]
        [if [n . > 0]
          [List.drop xs [n . I32.- 1]]
          xs
        ]
      ]
    ]
  ]]

  [def-decl pub List.is_length_at_least [-> [type A *] [List A] I32 Bool]]
  [def-impl List.is_length_at_least [\ [type A *] self n [do
    [if [n . > 0]
      [self . List.drop [n . I32.- 1] . List.is_empty . Bool.not]
      false
    ]
  ]]]

  [def-decl pub List.is_one [-> [type A *] [List A] Bool]]
  [def-impl List.is_one [\ [type A *] self
    [match self
      [[Cons [; x [Nil]]] true]
      [_ false]
    ]
  ]]

  [def-decl pub List.from_foldable [-> [type F] [type A] [F A] [auto [Foldable F]] [List A]]]
  [def-impl List.from_foldable [\ [type F] [type A] f.a [auto F.Foldable]
    [f.a . Foldable.foldr [List.Nil] [List.Cons]]
  ]]

  [def-decl pub List.iter [-> [type A] [List A] [Iter A]]]
  [def-impl List.iter [\ [type A] self [do
    [let list [var self]]
    [Iter.new [\ _ [do
      [match [get list]
        [[Nil] [do
          [None]
        ]]
        [[Cons [; x xs]] [do
          [set list xs]
          [Some x]
        ]]
      ]
    ]]]
  ]]]

  [def-decl pub List.from_iter [-> [type A] [Iter A] [List A]]]
  [def-impl List.from_iter [\ [type A] self [do
    [let items [var [List.Nil]]]
    [let a [self . Iter.foreach [\ x [do
      [update items [List.Cons x]]
      continue
    ]]]]
    [get items]
  ]]]

  // Highly efficient. This is the best way to convert an Array to a List, and
  // is way faster than 'List.from_foldable' or 'List.from_beam'.
  [def-decl pub List.from_array [-> [type A] [Array A] [List A]]]
  [def-impl List.from_array [\ [type A] arr [do
    [let list [var [List.Nil]]]
    [arr . Array.foreach_reversed [\ item [do
      [set list [get list . List.cons item]]
      continue
    ]]]
    [get list]
  ]]]

  // Alias to 'List.from_array'
  [def-decl pub Array.to_list [-> [type A] [Array A] [List A]]]
  [def-impl Array.to_list List.from_array]

  // * Base library: DList.
  //
  // See https://hackage.haskell.org/package/dlist.
  //
  // Like List but has O(1) append, cons, snoc and O(N) .toList(). Extremely
  // useful for aggregating a bunch of items and later be collected into a list.
  //
  // See https://hackage.haskell.org/package/dlist, but our implementation uses
  // impure functions under the hood for speed, and it happens so that there is
  // NO WAY to implement a '.iter()' for this particular 'DList' implementation
  // unless you use '.to_array()', etc.

  [class-decl DList [A *]]
  [class-struct DList
    // Emits the list of items.
    [field foreach [-> [-> A ,] ,]]
  ]

  [def-decl pub DList.<> [-> [type A] [DList A] [DList A] [DList A]]]
  [def-impl DList.<> [\ [type A] xs ys
    [DList.new [\ yield [do
      [xs . DList.foreach yield]
      [ys . DList.foreach yield]
    ]]]
  ]]

  [def-decl pub DList.empty [-> [type A] [DList A]]]
  [def-impl DList.empty [\ [type A] [DList.new [\ yield ,]]]]

  // Alias to 'DList.empty'
  [def-decl pub DList.nil [-> [type A] [DList A]]]
  [def-impl DList.nil DList.empty]

  [def-decl pub DList.cons [-> [type A] [DList A] A [DList A]]]
  [def-impl DList.cons [\ [type A] self x
    [DList.new [\ yield [do
      [yield x]
      [self . DList.foreach yield]
    ]]]
  ]]

  [def-decl pub DList.snoc [-> [type A] [DList A] A [DList A]]]
  [def-impl DList.snoc [\ [type A] self x
    [DList.new [\ yield [do
      [self . DList.foreach yield]
      [yield x]
    ]]]
  ]]

  [def-decl pub DList.one [-> [type A] A [DList A]]]
  [def-impl DList.one [\ [type A] x [DList.new [\ yield [yield x]]]]]

  [def-decl pub DList.map [-> [type A] [type B] [DList A] [-> A B] [DList B]]]
  [def-impl DList.map [\ [type A] [type B] self a->b
    [DList.new [\ yield [do
      [self . DList.foreach [\ a [yield [a->b a]]]]
    ]]]
  ]]

  [def-decl pub auto DList.Semigroup [-> [type A] [Semigroup [DList A]]]]
  [def-impl DList.Semigroup [\ [type A] [Semigroup.new [DList.<>]]]]

  [def-decl pub auto DList.Neutral [-> [type A] [Neutral [DList A]]]]
  [def-impl DList.Neutral [\ [type A] [Neutral.new [DList.empty]]]]

  [def-decl pub auto DList.Foldable [Foldable DList]]
  [def-impl DList.Foldable [Foldable.new [\ [type A] [type S] self state step [do
    [let st [var state]]
    [self . DList.foreach [\ a [do
      [set st [step [get st] a]]
    ]]]
    [get st]
  ]]]]

  [def-decl pub DList.to_list [-> [type A] [DList A] [List A]]]
  [def-impl DList.to_list [\ [type A] self [do
    [List.from_foldable self]
  ]]]

  [def-decl pub DList.to_array [-> [type A] [DList A] [Array A]]]
  [def-impl DList.to_array [\ [type A] self [do
    [let xs [Array.create ,]]
    [self . DList.foreach [\ x [xs . Array.append x]]]
    xs
  ]]]

  // The DList returns whatever the Array has at the moment of 'DList.foreach',
  // therefore mutating the input array afterwards will change the DList's
  // output.
  [def-decl pub DList.from_array [-> [type A] [Array A] [DList A]]]
  [def-impl DList.from_array [\ [type A] arr [do
    [DList.new [\ yield [do
      [arr . Array.foreach [\ a [do
        [yield a]
        continue
      ]]]
    ]]]
  ]]]
  
  // * Base library: Char

  [def-decl pub Char.== [-> Char Char Bool]]
  [def-annotate Char.== "foreign-import:char_equal"]

  [def-decl pub auto Char.Eq [Eq Char]]
  [def-impl Char.Eq [Eq.new Char.==]]

  [def-decl pub Char.!= [-> Char Char Bool]]
  [def-impl Char.!= [\ a b [Bool.not [Char.== a b]]]]

  [def-decl pub Char.ord [-> Char I32]]
  [def-annotate Char.ord "foreign-import:char_to_ord"]

  [def-decl pub Char.chr [-> I32 Char]]
  [def-annotate Char.chr "foreign-import:char_from_ord"]

  [def-decl pub Char.is_alpha [-> Char Bool]]
  [def-annotate Char.is_alpha "foreign-import:char_is_alpha"]

  [def-decl pub Char.is_alpha_num [-> Char Bool]]
  [def-annotate Char.is_alpha_num "foreign-import:char_is_alpha_num"]

  [def-decl pub Char.is_space [-> Char Bool]]
  [def-annotate Char.is_space  "foreign-import:char_is_space"]

  // Is ASCII '0'-'9'?
  [def-decl pub Char.is_ascii_digit [-> Char Bool]]
  [def-impl Char.is_ascii_digit [\ ch
    [Bool.and
      [[Char.ord '0'] . <= [Char.ord ch]]
      [[Char.ord ch] . <= [Char.ord '9']]
    ]
  ]]

  [def-decl pub Char.parse_ascii_digit [-> Char [Maybe I32]]]
  [def-impl Char.parse_ascii_digit [\ ch
    [if [Char.is_ascii_digit ch]
      [Some [Char.ord ch . I32.- [Char.ord '0']]]
      [None]
    ]
  ]]

  // * Base library: (Immutable) String.
  //
  // Behaves like JavaScript/Python strings: immutable blocks of UTF-8 text with
  // O(1) .length(), .at(), .slice(), etc.
  //
  // Behaves like a Haskell 'Data.Vector.Unboxed Char' (NOT a 'Data.Text').

  [def-decl pub String.from_char [-> Char String]]
  [def-annotate String.from_char "foreign-import:string_from_char"]

  [def-decl pub String.== [-> String String Bool]]
  [def-annotate String.== "foreign-import:string_equal"]

  [def-decl pub auto String.Eq [Eq String]]
  [def-impl String.Eq [Eq.new String.==]]

  [def-decl pub String.compare [-> [type A] String String A A A A]]
  [def-annotate String.compare "foreign-import:string_compare"]

  [def-decl pub auto String.Ord [Ord String]]
  [def-impl String.Ord [Ord.new [\ a b [String.compare a b Ordering.LT Ordering.EQ Ordering.GT]]]]

  [def-decl pub String.length [-> String I32]]
  [def-annotate String.length "foreign-import:string_length"]

  [def-decl pub String.is_empty [-> String Bool]]
  [def-annotate String.is_empty "foreign-import:string_is_empty"]

  [def-decl pub String.cons [-> Char String String]]
  [def-annotate String.cons "foreign-import:string_cons"]

  [def-decl pub String.at [-> String I32 Char]]
  [def-annotate String.at "foreign-import:string_at"]

  [def-decl pub String.from_array [-> [Array Char] String]]
  [def-annotate String.from_array "foreign-import:string_from_array"]

  [def-decl pub String.at? [-> String I32 [Maybe Char]]]
  [def-impl String.at? [\ str index [do
    [if [index . < [str . String.length]]
      [Some [str . String.at index]]
      [None]
    ]
  ]]]

  [def-decl pub String.slice [-> String I32 I32 String]]
  [def-annotate String.slice "foreign-import:string_slice"]

  [def-decl pub String.reverse [-> String String]]
  [def-annotate String.reverse "foreign-import:string_reverse"]

  [def-decl pub auto String.Semigroup [Semigroup String]]
  [def-impl String.Semigroup [Semigroup.new String.<>]]

  [def-decl pub auto String.Neutral [Neutral String]]
  [def-impl String.Neutral [Neutral.new ""]]

  // Find from left to right starting from a position. Returns -1 if not found.
  //
  // Works in O(N) time, where N is the length of 'str'.
  [def-decl String.ffi_find [-> String I32 String I32]]
  [def-annotate String.ffi_find "foreign-import:string_find"]

  [def-decl pub String.find_from [-> String I32 String [Maybe I32]]]
  [def-impl String.find_from [\ self start str [do
    [let i [self . String.ffi_find start str]]
    [if [i . == -1] [None] [Some i]]
  ]]]

  [def-decl pub String.find [-> String String [Maybe I32]]]
  [def-impl String.find [\ self [String.find_from self 0]]]

  // Works in O(N) time
  [def-decl pub String.contains [-> String String Bool]]
  [def-impl String.contains [\ self str [self . String.find str . Maybe.is_none]]]

  // If length is not positive, no-op.
  [def-decl pub String.take [-> String I32 String]]
  [def-impl String.take [\ str n [do
    [let len [str . String.length]]
    [str . String.slice 0 [I32.min n len]]
  ]]]

  // If length is not positive, no-op.
  [def-decl pub String.drop [-> String I32 String]]
  [def-impl String.drop [\ str n [do
    [let len [str . String.length]]
    [str . String.slice [I32.min n len] len]]
  ]]

  // If length is not positive, no-op.
  [def-decl pub String.take_end [-> String I32 String]]
  [def-impl String.take_end [\ str n [do
    [String.drop str [str . String.length . I32.- n]]
  ]]]

  // If length is not positive, no-op.
  [def-decl pub String.drop_end [-> String I32 String]]
  [def-impl String.drop_end [\ str n [do
    [String.take str [str . String.length . I32.- n]]
  ]]]

  [def-decl pub String.take_and_drop [-> String I32 [; String String]]]
  [def-impl String.take_and_drop [\ str n [do
    [let len [str . String.length]]
    [; [str . String.take n] [str . String.drop n]]
  ]]]

  // Returns the number of characters from the start of the string that satisfy
  // the predicate.
  [def-decl pub String.count_span [-> String [-> Char Bool] I32]]
  [def-impl String.count_span [\ self predicate [do
    [let len [self . String.length]]

    [let*
      [go [\ [i I32] [do
        [let= [false] [i . == len]
          [_ len]]

        [let= [true] [self . String.at i . predicate]
          [_ i]]

        [go [i . I32.suc]]
      ]]]
    ]

    [go 0]
  ]]]

  // Returns the number of characters from the end of the string that satisfy
  // the predicate.
  [def-decl pub String.count_span_end [-> String [-> Char Bool] I32]]
  [def-impl String.count_span_end [\ self predicate [do
    [let len [self . String.length]]

    [let*
      [go [\ [i I32] [do
        [let= [false] [i . == -1]
          [_ len]]

        [let= [true] [self . String.at i . predicate]
          [_ [len . I32.- i . I32.- 1]]]

        [go [i . I32.dec]]
      ]]]
    ]

    [go [I32.dec len]]
  ]]]

  // Returns the prefix and suffix where the prefix is the longest prefix of the
  // string that satisfies the predicate. The suffix is the rest of the string.
  //
  // Returns as (matched prefix, suffix)
  [def-decl pub String.span [-> String [-> Char Bool] [; String String]]]
  [def-impl String.span [\ self predicate [do
    [self . String.take_and_drop [self . String.count_span predicate]]
  ]]]

  // Same as 'String.span' but finds the longest suffix instead of the longest
  // prefix.
  //
  // Returns as (prefix, matched suffix)
  [def-decl pub String.span_end [-> String [-> Char Bool] [; String String]]]
  [def-impl String.span_end [\ self predicate [do
    [self . String.take_and_drop [self . String.count_span_end predicate]]
  ]]]

  [def-decl pub String.take_while [-> String [-> Char Bool] String]]
  [def-impl String.take_while [\ str predicate [do
    [str . String.take [str . String.count_span predicate]]
  ]]]

  [def-decl pub String.drop_while [-> String [-> Char Bool] String]]
  [def-impl String.drop_while [\ str predicate [do
    [str . String.drop [str . String.count_span predicate]]
  ]]]

  [def-decl pub String.take_end_while [-> String [-> Char Bool] String]]
  [def-impl String.take_end_while [\ str predicate [do
    [str . String.take_end [str . String.count_span_end predicate]]
  ]]]

  [def-decl pub String.drop_end_while [-> String [-> Char Bool] String]]
  [def-impl String.drop_end_while [\ str predicate [do
    [str . String.drop_end [str . String.count_span_end predicate]]
  ]]]

  [def-decl pub String.starts_with [-> String String Bool]]
  [def-impl String.starts_with [\ self prefix
    [self . String.take [prefix . String.length] . String.== prefix]
  ]]

  [def-decl pub String.ends_with [-> String String Bool]]
  [def-impl String.ends_with [\ self suffix
    [self . String.take_end [suffix . String.length] . String.== suffix]
  ]]

  [def-decl pub String.trim_left [-> String String]]
  [def-impl String.trim_left [\ str [str . String.drop_while Char.is_space]]]

  [def-decl pub String.trim_right [-> String String]]
  [def-impl String.trim_right [\ str [str . String.drop_end_while Char.is_space]]]

  [def-decl pub String.trim [-> String String]]
  [def-impl String.trim [\ str [str . String.trim_left . String.trim_right]]]

  // Returns the suffix after the prefix if the string starts with the prefix,
  // otherwise None.
  [def-decl pub String.strip_prefix? [-> String String [Maybe String]]]
  [def-impl String.strip_prefix? [\ self prefix [do
    [if [self . String.starts_with prefix]
      [Some [self . String.drop [prefix . String.length]]]
      [None]
    ]
  ]]]

  [def-decl pub String.foreach [-> String [-> Char ShouldContinue] ,]]
  [def-impl String.foreach [\ self callback
    [loop<n [self . String.length] [\ i
      [callback [self . String.at i]]
    ]]
  ]]

  [def-decl pub String.iter_on_range [-> String [-> [Range I32] [Range I32]] [Iter Char]]]
  [def-impl String.iter_on_range [\ self mod_range [do
    [Range.to [self . String.length]
      . mod_range
      . Range.iter
      . Iter.map [String.at self]
    ]
  ]]]

  [def-decl pub String.iter [-> String [Iter Char]]]
  [def-impl String.iter [\ str [str . String.iter_on_range [fn.id]]]]

  [def-decl pub String.concat_iter [-> [Iter String] String]]
  [def-impl String.concat_iter [\ strings [do
    [let b [StringBuilder.create ,]]
    [strings . Iter.foreach [\ s [do
      [b . StringBuilder.append s]
      continue
    ]]]
    [b . StringBuilder.build]
  ]]]

  // If keep_end? is false with input "a\n\nb\nc", this yields ["a", "", "b", "c"] (without trailing
  // newline characters)
  //
  // If keep_end? is true, this yields ["a\n", "\n", "b\n", "c"] (with trailing newline characters) instead.
  [def-decl String.iter_lines_impl [-> String Bool [Iter String]]]
  [def-impl String.iter_lines_impl [\ self keep_end? [Gen.iter [do
    [let len [self . String.length]]

    [let*
      [go [\ [start I32] [i I32] [:: [type [Gen String ,]] [do
        [if-let [true] [i . == len] [do
          [Gen.yield [self . String.slice start len]]
        ]]

        [let i* [I32.suc i]]
        [if [self . String.at i . == '\n']
          [do
            [try _ [Gen.yield [self . String.slice start [if keep_end? i* i]]]]
            [go i* i*]
          ]
          [do
            [go start i*]
          ]
        ]
      ]]]]
    ]
    [go 0 0]
  ]]]]

  // Given "a\n\nb\nc", this yields ["a", "", "b", "c"] (without trailing
  // newline characters)
  [def-decl String.iter_lines [-> String [Iter String]]]
  [def-impl String.iter_lines [\ self [self . String.iter_lines_impl false]]]

  // Given "a\n\nb\nc", this yields ["a\n", "\n", "b\n", "c"] (with trailing
  // newline characters).
  [def-decl String.iter_lines* [-> String [Iter String]]]
  [def-impl String.iter_lines* [\ self [self . String.iter_lines_impl true]]]

  // Inspired by
  // https://hackage-content.haskell.org/package/text-2.1.4/docs/Data-Text.html#v:concatMap
  [def-decl pub String.concat_map [-> String [-> Char String] String]]
  [def-impl String.concat_map [\ str f [do
    [let b [StringBuilder.create ,]]
    [str . String.foreach [\ c [do
      [b . StringBuilder.append [f c]]
      continue
    ]]]
    [b . StringBuilder.build]
  ]]]

  // Inspired by https://hackage-content.haskell.org/package/text-2.1.4/docs/Data-Text.html#v:map
  [def-decl pub String.map [-> String [-> Char Char] String]]
  [def-impl String.map [\ str f [do
    [let b [StringBuilder.create ,]]
    [str . String.foreach [\ c [do
      [b . StringBuilder.append_char [f c]]
      continue
    ]]]
    [b . StringBuilder.build]
  ]]]

  // Returns true if all characters in the string are whitespace characters.
  [def-decl pub String.is_space [-> String Bool]]
  [def-impl String.is_space [\ str [do
    [== [str . String.count_span Char.is_space] [str . String.length]]
  ]]]

  // Works like https://docs.python.org/3/library/textwrap.html#textwrap.dedent
  [def-decl pub String.dedent [-> String String]]
  [def-impl String.dedent [\ str [do
    // TODO: Document more details and edge cases
    //
    // FIXME: Incomplete implementation. See
    // https://docs.python.org/3/library/textwrap.html#textwrap.dedent

    [let lines [str . String.iter_lines* . Array.from_iter]]

    // Compute 'least_indent'
    [let least_indent [var [None]]]
    [lines . Array.foreach [\ line [do
      [let indent [line . String.count_span Char.is_space]]
      [let is_str_space [== indent [String.length line]]]
      [unless is_str_space [\ _ [do
        [match [get least_indent]
          [[None] [do
            [set least_indent [Some indent]]
          ]]
          [[Some current_least] [do
            [when [indent . < current_least] [\ _ [do
              [set least_indent [Some indent]]
            ]]]
          ]]
        ]
      ]]]
      continue
    ]]]

    // Edge case: The entire string is either empty or consists of whitespace
    // characters only.
    [let= [Some least_indent] [get least_indent]
      [_ str]]

    // Dedent all lines
    [let b [StringBuilder.create ,]]
    [lines . Array.foreach [\ line [do
      // NOTE: Due to 'String.iter_lines*', the line contains the trailing
      // newline character(s) if any.
      [b . StringBuilder.append [line . String.drop least_indent]]
      continue
    ]]]
    [b . StringBuilder.build]
  ]]]

  // * Base library: Immutable & Mutable I32Map
  //
  // API is backed by the host environment for performance reasons.

  [class-decl I32Map [A *]]
  [class-decl I32MutMap [A *]]

  [def-decl pub I32Map.empty [-> [type A *] [I32Map A]]]
  [def-annotate I32Map.empty "foreign-import:i32_map_empty"]

  [def-decl pub I32Map.has [-> [type A *] [I32Map A] I32 Bool]]
  [def-annotate I32Map.has "foreign-import:i32_map_has"]

  [def-decl pub I32Map.set [-> [type A *] [I32Map A] I32 A [I32Map A]]]
  [def-annotate I32Map.set "foreign-import:i32_map_set"]

  [def-decl pub I32Map.get [-> [type A *] [I32Map A] I32 A]]
  [def-annotate I32Map.get "foreign-import:i32_map_get"]

  [def-decl pub I32Map.get? [-> [type A *] [I32Map A] I32 [Maybe A]]]
  [def-impl I32Map.get? [\ [type A *] self key [do
    [if [self . I32Map.has key]
      [Some [self . I32Map.get key]]
      [None]
    ]
  ]]]

  [def-decl pub I32Map.one [-> [type A] I32 A [I32Map A]]]
  [def-impl I32Map.one [\ [type A] key value
    [I32Map.empty . I32Map.set key value]
  ]]

  [def-decl pub I32MutMap.create [-> [type A *] , [I32MutMap A]]]
  [def-annotate I32MutMap.create "foreign-import:i32_mut_map_create"]

  [def-decl pub I32MutMap.freeze [-> [type A *] [I32MutMap A] [I32Map A]]]
  [def-annotate I32MutMap.freeze "foreign-import:i32_mut_map_freeze"]

  [def-decl pub I32MutMap.has [-> [type A *] [I32MutMap A] I32 Bool]]
  [def-annotate I32MutMap.has "foreign-import:i32_mut_map_has"]

  [def-decl pub I32MutMap.set [-> [type A *] [I32MutMap A] I32 A ,]]
  [def-annotate I32MutMap.set "foreign-import:i32_mut_map_set"]

  [def-decl pub I32MutMap.get [-> [type A *] [I32MutMap A] I32 A]]
  [def-annotate I32MutMap.get "foreign-import:i32_mut_map_get"]

  [def-decl pub I32MutMap.get? [-> [type A] [I32MutMap A] I32 [Maybe A]]]
  [def-impl I32MutMap.get? [\ [type A] self key
    [if [self . I32MutMap.has key]
      [Some [self . I32MutMap.get key]]
      [None]
    ]
  ]]

  [def-decl I32MutMap.create_host_iterator [-> [type A] [I32MutMap A] [HostIterator [; I32 A]]]]
  [def-annotate I32MutMap.create_host_iterator "foreign-import:i32_mut_map_create_host_iterator"]

  [def-decl pub I32MutMap.iter [-> [type A] [I32MutMap A] [Iter [; I32 A]]]]
  [def-impl I32MutMap.iter [\ [type A] self
    [self . I32MutMap.create_host_iterator . HostIterator.to_iter]
  ]]

  [def-decl pub I32MutMap.iter_values [-> [type A] [I32MutMap A] [Iter A]]]
  [def-impl I32MutMap.iter_values [\ [type A] self
    [self . I32MutMap.iter . Iter.map [Pair.snd]]
  ]]

  // * Base library: Immutable & Mutable I32Set
  //
  // API is backed by the host environment for performance reasons.

  [class-decl I32Set]
  [class-decl I32MutSet]

  [def-decl pub I32Set.empty [I32Set]]
  [def-annotate I32Set.empty "foreign-import:i32_set_empty"]

  [def-decl pub I32Set.has [-> I32Set I32 Bool]]
  [def-annotate I32Set.has "foreign-import:i32_set_has"]

  [def-decl pub I32Set.add [-> I32Set I32 I32Set]]
  [def-annotate I32Set.add "foreign-import:i32_set_add"]

  [def-decl pub I32MutSet.create [-> , I32MutSet]]
  [def-annotate I32MutSet.create "foreign-import:i32_mut_set_create"]

  [def-decl pub I32MutSet.has [-> I32MutSet I32 Bool]]
  [def-annotate I32MutSet.has "foreign-import:i32_mut_set_has"]

  [def-decl pub I32MutSet.add [-> I32MutSet I32 ,]]
  [def-annotate I32MutSet.add "foreign-import:i32_mut_set_add"]

  [def-decl pub I32MutSet.remove [-> I32MutSet I32 ,]]
  [def-annotate I32MutSet.remove "foreign-import:i32_mut_set_remove"]

  [def-decl pub I32MutSet.freeze [-> I32MutSet I32Set]]
  [def-annotate I32MutSet.freeze "foreign-import:i32_mut_set_freeze"]

  [def-decl pub I32MutSet.is_empty [-> I32MutSet Bool]]
  [def-annotate I32MutSet.is_empty "foreign-import:i32_mut_set_is_empty"]

  // Get an arbitrary element from the set up to implementation details.
  // Behavior is undefined if the set is empty.
  [def-decl pub I32MutSet.choose [-> I32MutSet I32]]
  [def-annotate I32MutSet.choose "foreign-import:i32_mut_set_choose"]

  [def-decl pub I32MutSet.choose? [-> I32MutSet [Maybe I32]]]
  [def-impl I32MutSet.choose? [\ self [do
    [if [self . I32MutSet.is_empty]
      [None]
      [Some [self . I32MutSet.choose]]]
  ]]]

  [def-decl pub I32MutSet.from_iter [-> [Iter I32] I32MutSet]]
  [def-impl I32MutSet.from_iter [\ iter [do
    [let self [I32MutSet.create ,]]
    [iter . Iter.foreach [\ x [do
      [self . I32MutSet.add x]
      continue
    ]]]
    self
  ]]]

  [def-decl I32MutSet.create_host_iterator [-> I32MutSet [HostIterator I32]]]
  [def-annotate I32MutSet.create_host_iterator "foreign-import:i32_mut_set_create_host_iterator"]

  [def-decl pub I32MutSet.iter [-> I32MutSet [Iter I32]]]
  [def-impl I32MutSet.iter [\ self
    [self . I32MutSet.create_host_iterator . HostIterator.to_iter]
  ]]

  // * Base library: Immutable & Mutable String Set
  //
  // API is backed by the host environment for performance reasons.

  [class-decl StringSet]

  [def-decl pub StringSet.empty StringSet]
  [def-annotate StringSet.empty "foreign-import:string_set_empty"]

  [def-decl pub StringSet.freeze [-> StringSet StringSet]]
  [def-annotate StringSet.freeze "foreign-import:string_set_freeze"]

  // Checks if two sets are equivalent.
  [def-decl pub StringSet.== [-> StringSet StringSet Bool]]
  [def-annotate StringSet.== "foreign-import:string_set_equal"]

  [def-decl pub auto Eq.StringSet [Eq StringSet]]
  [def-impl Eq.StringSet [Eq.new StringSet.==]]

  [def-decl pub StringSet.has [-> StringSet String Bool]]
  [def-annotate StringSet.has "foreign-import:string_set_has"]

  [def-decl pub StringSet.add [-> StringSet String StringSet]]
  [def-annotate StringSet.add "foreign-import:string_set_add"]

  [def-decl pub StringSet.remove [-> StringSet String StringSet]]
  [def-annotate StringSet.remove "foreign-import:string_set_remove"]

  [def-decl pub StringSet.intersect [-> StringSet StringSet StringSet]]
  [def-annotate StringSet.intersect "foreign-import:string_set_intersect"]

  [def-decl pub StringSet.union [-> StringSet StringSet StringSet]]
  [def-annotate StringSet.union "foreign-import:string_set_union"]

  [def-decl StringSet.create_host_iterator [-> StringSet [HostIterator String]]]
  [def-annotate StringSet.create_host_iterator "foreign-import:string_set_create_host_iterator"]

  [def-decl pub StringSet.iter [-> StringSet [Iter String]]]
  [def-impl StringSet.iter [\ self [self . StringSet.create_host_iterator . HostIterator.to_iter]]]

  // * Base library: Immutable & Mutable String Map
  //
  // API is backed by the host environment for performance reasons.

  [class-decl StringMap [A *]]
  [class-decl StringMutMap [A *]]

  [def-decl pub StringMap.empty [-> [type A *] [StringMap A]]]
  [def-annotate StringMap.empty "foreign-import:string_map_empty"]

  [def-decl pub StringMap.set [-> [type A *] [StringMap A] String A [StringMap A]]]
  [def-annotate StringMap.set "foreign-import:string_map_set"]

  [def-decl pub StringMap.has [-> [type A *] [StringMap A] String Bool]]
  [def-annotate StringMap.has "foreign-import:string_map_has"]

  [def-decl pub StringMap.get [-> [type A *] [StringMap A] String A]]
  [def-annotate StringMap.get "foreign-import:string_map_get"]

  [def-decl pub StringMap.get? [-> [type A *] [StringMap A] String [Maybe A]]]
  [def-impl StringMap.get? [\ [type A *] self key [do
    [if [self . StringMap.has key]
      [Some [self . StringMap.get key]]
      [None]
    ]
  ]]]

  [def-decl pub StringMap.keys_set [-> [type A *] [StringMap A] StringSet]]
  [def-annotate StringMap.keys_set "foreign-import:string_map_keys_set"]

  // Iterates through all key-value pairs. Output order is unspecified and is
  // dependent on the host environment.
  [def-decl StringMap.create_host_iterator [-> [type A *] [StringMap A] [HostIterator [; String A]]]]
  [def-annotate StringMap.create_host_iterator "foreign-import:string_map_create_host_iterator"]

  [def-decl pub StringMap.iter [-> [type A] [StringMap A] [Iter [; String A]]]]
  [def-impl StringMap.iter [\ [type A] self [self . StringMap.create_host_iterator . HostIterator.to_iter]]]

  [def-decl pub StringMap.iter_values [-> [type A] [StringMap A] [Iter A]]]
  [def-impl StringMap.iter_values [\ [type A] self
    [self . StringMap.iter . Iter.map [Pair.snd]]
  ]]

  [def-decl pub StringMutMap.create [-> [type A *] , [StringMutMap A]]]
  [def-annotate StringMutMap.create "foreign-import:string_mut_map_create"]

  [def-decl pub StringMutMap.set [-> [type A *] [StringMutMap A] String A ,]]
  [def-annotate StringMutMap.set "foreign-import:string_mut_map_set"]

  [def-decl pub StringMutMap.has [-> [type A *] [StringMutMap A] String Bool]]
  [def-annotate StringMutMap.has "foreign-import:string_mut_map_has"]

  [def-decl pub StringMutMap.get [-> [type A *] [StringMutMap A] String A]]
  [def-annotate StringMutMap.get "foreign-import:string_mut_map_get"]

  [def-decl pub StringMutMap.get? [-> [type A *] [StringMutMap A] String [Maybe A]]]
  [def-impl StringMutMap.get? [\ [type A *] self key
    [if [self . StringMutMap.has key]
      [Some [self . StringMutMap.get key]]
      [None]
    ]
  ]]

  [def-decl pub StringMutMap.freeze [-> [type A *] [StringMutMap A] [StringMap A]]]
  [def-annotate StringMutMap.freeze "foreign-import:string_mut_map_freeze"]

  // Works like Python's 'dict.update()' method.
  [def-decl pub StringMutMap.update [-> [type A *] [StringMutMap A] [StringMutMap A] ,]]
  [def-annotate StringMutMap.update "foreign-import:string_mut_map_update"]

  [def-decl pub StringMap.map_maybe_with_key [-> [type A *] [type B *] [StringMap A] [-> String A [Maybe B]] [StringMap B]]]
  [def-impl StringMap.map_maybe_with_key [\ [type A *] [type A *] map f [do
    [let out [StringMutMap.create ,]]
    [map . StringMap.iter . Iter.foreach [\ pair [do
      [let= [; key a] pair]
      [match [f key a]
        [[Some b] [out . StringMutMap.set key b]]
        [[None] ,]
      ]
      continue
    ]]]
    [out . StringMutMap.freeze]
  ]]]

  [def-decl pub StringMap.map [-> [type A *] [type B *] [StringMap A] [-> A B] [StringMap B]]]
  [def-impl StringMap.map [\ [type A *] [type B *] map f
    [map . StringMap.map_maybe_with_key [\ _ a [Some [f a]]]]
  ]]

  [def-decl pub StringMap.merge_with_key [-> [type A] [type B] [type C] [StringMap A] [StringMap B] [-> String [These A B] [Maybe C]] [StringMap C]]]
  [def-impl StringMap.merge_with_key [\ [type A] [type B] [type C] xs ys merger [do
    [let xs_keys [xs . StringMap.keys_set]]
    [let ys_keys [ys . StringMap.keys_set]]
    [let all_keys [xs_keys . StringSet.union ys_keys]]

    [let out [StringMutMap.create ,]]
    [all_keys . StringSet.iter . Iter.foreach [\ key [do
      [let a [xs . StringMap.get? key]]
      [let b [ys . StringMap.get? key]]
      [let these [match [; a b]
        [[; [Some a] [None]]
          [These.L a]]
        [[; [None] [Some b]]
          [These.R b]]
        [[; [Some a] [Some b]]
          [These.T a b]]
        [[; [None] [None]]
          [panic ["impossible: key '" . <> key . <> "' is unexpectedly absent"]]]
      ]]
      [match [merger key these]
        [[None] ,]
        [[Some c] [out . StringMutMap.set key c]]
      ]
      continue
    ]]]
    [out . StringMutMap.freeze]
  ]]]

  [def-decl pub auto StringMap.Semialign [Semialign StringMap]]
  [def-impl StringMap.Semialign [Semialign.new
    [\ [type A] [type B] [type C] xs ys aligner [StringMap.merge_with_key xs ys [\ _key input
      [Some [aligner input]]
    ]]]
  ]]

  [def-decl pub StringMap.traverse_with_key [-> [type A] [type B] [type M] [StringMap A] [-> String A [M B]] [auto [Monad M]] [M [StringMap B]]]]
  [def-impl StringMap.traverse_with_key [\ [type A] [type B] [type M] map callback [auto M.Monad] [do
    [let out [StringMutMap.create ,]]
    [try _ [map . StringMap.iter . Iter.traverse_ [\ pair [do
      [let= [; key a] pair]
      [try b [callback key a]]
      [out . StringMutMap.set key b]
      [pure continue]
    ]]]]
    [pure [out . StringMutMap.freeze]]
  ]]]

  [def-decl pub StringMap.traverse [-> [type A] [type B] [type M] [StringMap A] [-> A [M B]] [auto [Monad M]] [M [StringMap B]]]]
  [def-impl StringMap.traverse [\ [type A] [type B] [type M] map callback [auto M.Monad] [do
    [map . StringMap.traverse_with_key [\ _key callback]]
  ]]]

  // * Base library: String escape

  [def-decl String.escape_like_json.table [I32Map String]]
  [def-impl String.escape_like_json.table [do
    [let map [I32MutMap.create ,]]
    [I32MutMap.set map [Char.ord '"'] "\\\""]
    [I32MutMap.set map [Char.ord '\\'] "\\\\"]
    [I32MutMap.set map [Char.ord '\b'] "\\n"]
    [I32MutMap.set map [Char.ord '\f'] "\\f"]
    [I32MutMap.set map [Char.ord '\n'] "\\n"]
    [I32MutMap.set map [Char.ord '\r'] "\\r"]
    [I32MutMap.set map [Char.ord '\t'] "\\t"]
    [map . I32MutMap.freeze]
  ]]

  // Escape a string as a JSON string literal (without the surrounding double quotes),
  // output is written to the given 'StringBuilder'.
  [def-decl pub String.escape_like_json* [-> String StringBuilder ,]]
  [def-impl String.escape_like_json* [\ self b [do
    // TODO: Optimize
    [self . String.foreach [\ ch [do
      // NOTE: See https://www.json.org/json-en.html
      //
      // NOTE: We don't escape forward slash (just like 'JSON.stringify()')

      [if-let [Some string] [String.escape_like_json.table . I32Map.get? [ch . Char.ord]] [do
        [b . StringBuilder.append string]
        continue
      ]]

      // FIXME: Handle U+0000 to U+001F & '\uXXXX'
      [b . StringBuilder.append_char ch]
      continue
    ]]]
  ]]]

  // Like applying JavaScript 'JSON.stringify()' on the 'String'
  [def-decl pub String.stringify_as_json [-> String String]]
  [def-impl String.stringify_as_json [\ self [do
    [let b [StringBuilder.create ,]]
    [b . StringBuilder.append_char '"']
    [self . String.escape_like_json* b]
    [b . StringBuilder.append_char '"']
    [b . StringBuilder.build]
  ]]]

  // * Panic variants inspired by Rust

  // Like Rust's 'todo!()' macro.
  [def-decl pub TODO [-> [type A] String A]]
  [def-impl TODO [\ [type A] msg [panic ["reached TODO: " . <> msg]]]]

  // Like Rust's 'unimplemented!()' macro.
  [def-decl pub UNIMPLEMENTED [-> [type A] String A]]
  [def-impl UNIMPLEMENTED [\ [type A] msg [panic ["reached UNIMPLEMENTED: " . <> msg]]]]

  // Like Rust's 'unreachable!()' macro.
  [def-decl pub UNREACHABLE [-> [type A] String A]]
  [def-impl UNREACHABLE [\ [type A] msg [panic ["reached UNREACHABLE: " . <> msg]]]]

  // * Base library: Debug
  //
  // Like Rust's Debug trait or Haskell's Show

  [class-decl Debug [A *]]
  [class-struct Debug
    [field on_format_to [-> A StringBuilder ,]]
  ]

  [def-decl pub Debug.format_to [-> [type A *] A StringBuilder [auto [Debug A]] ,]]
  [def-impl Debug.format_to [\ [type A *] self out [auto A.Debug] [do
    [A.Debug . Debug.on_format_to self out]
  ]]]

  [def-decl pub Debug.repr [-> [type A] A [auto [Debug A]] String]]
  [def-impl Debug.repr [\ [type A] self [auto A.Debug] [do
    [let out [StringBuilder.create ,]]
    [self . Debug.format_to out]
    [out . StringBuilder.build]
  ]]]

  [def-decl pub auto Unit.Debug [Debug ,]]
  [def-impl Unit.Debug [Debug.new 
    [\ _self out [out . StringBuilder.append ","]]
  ]]

  [def-decl pub auto Pair.Debug [-> [type A] [type B] [auto [Debug A]] [auto [Debug B]] [Debug [; A B]]]]
  [def-impl Pair.Debug [\ [type A] [type B] [auto A.Debug] [auto B.Debug] [Debug.new
    [\ self out [do
      [out . StringBuilder.append "("]
      [self . Pair.fst . Debug.format_to out]
      [self . Pair.snd . Debug.format_to out]
      [out . StringBuilder.append ")"]
    ]]
  ]]]

  [def-decl pub auto I32.Debug [Debug I32]]
  [def-impl I32.Debug [Debug.new
    [\ self out [out . StringBuilder.append [self . I32.to_str]]]
  ]]

  [def-decl pub auto Int.Debug [Debug Int]]
  [def-impl Int.Debug [Debug.new
    [\ self out [out . StringBuilder.append [self . Int.to_str]]]
  ]]

  [def-decl pub auto Bool.Debug [Debug Bool]]
  [def-impl Bool.Debug [Debug.new
    [\ self out [out . StringBuilder.append [self . Bool.to_str]]]
  ]]

  [def-decl pub auto Char.Debug [Debug Char]]
  [def-impl Char.Debug [Debug.new
    [\ self out [do
      [out . StringBuilder.append_char '\'']
      [out . StringBuilder.append_char self]
      [out . StringBuilder.append_char '\'']
    ]]
  ]]

  [def-decl pub auto String.Debug [Debug String]]
  [def-impl String.Debug [Debug.new
    [\ self out [do
      [out . StringBuilder.append_char '"']
      [self . String.escape_like_json* out]
      [out . StringBuilder.append_char '"']
    ]]
  ]]

  [def-decl pub auto Maybe.Debug [-> [type A *] [auto [Debug A]] [Debug [Maybe A]]]]
  [def-impl Maybe.Debug [\ [type A *] [auto A.Debug] [Debug.new [\ self out
    [match self
      [[Some x] [do
        [out . StringBuilder.append "Some("]
        [x . Debug.format_to out]
        [out . StringBuilder.append ")"]
      ]]
      [[None] [do
        [out . StringBuilder.append "None"]
      ]]
    ]
  ]]]]

  [def-decl pub auto Either.Debug [-> [type E *] [type A *] [auto [Debug E]] [auto [Debug A]] [Debug [Either E A]]]]
  [def-impl Either.Debug [\ [type E *] [type A *] [auto E.Debug] [auto A.Debug] [Debug.new
    [\ self out
      [match self
        [[L a] [do
          [out . StringBuilder.append "L("]
          [a . Debug.format_to out]
          [out . StringBuilder.append ")"]
        ]]
        [[R b] [do
          [out . StringBuilder.append "R("]
          [b . Debug.format_to out]
          [out . StringBuilder.append ")"]
        ]]
      ]
    ]
  ]]]

  [def-decl Debug.format_list_from_iter [-> [type A] StringBuilder [Iter A] [auto [Debug A]] ,]]
  [def-impl Debug.format_list_from_iter [\ [type A] out iter [auto A.Debug] [do
    [let first [var true]]
    [out . StringBuilder.append "["]
    [iter . Iter.foreach [\ item [do
      [if [first . get]
        [first . set false]
        [out . StringBuilder.append ", "]
      ]
      [item . Debug.format_to out]
      continue
    ]]]
    [out . StringBuilder.append "]"]
  ]]]

  [def-decl pub auto Array.Debug [-> [type A] [auto [Debug A]] [Debug [Array A]]]]
  [def-impl Array.Debug [\ [type A] [auto A.Debug] [Debug.new
    [\ self out [do
      [Debug.format_list_from_iter out [self . Array.iter]]
    ]]
  ]]]

  [def-decl pub auto Vec.Debug [-> [type A] [auto [Debug A]] [Debug [Vec A]]]]
  [def-impl Vec.Debug [\ [type A] [auto A.Debug] [Debug.new
    [\ self out [do
      [Debug.format_list_from_iter out [self . Vec.iter]]
    ]]
  ]]]

  [def-decl pub auto Seq.Debug [-> [type A] [auto [Debug A]] [Debug [Seq A]]]]
  [def-impl Seq.Debug [\ [type A] [auto A.Debug] [Debug.new
    [\ self out [do
      [Debug.format_list_from_iter out [self . Seq.iter]]
    ]]
  ]]]

  // * Base library: Unknown
  //
  // An opaque type for casting arbitrary values to. Used to escape from the
  // type system.

  [class-decl Unknown]

  // Turns a value of any type into an Unknown. This function does not crash
  // (but may panic of the backend does not support the input).
  [def-decl Unknown.of [-> [type A] A Unknown]]
  [def-annotate Unknown.of "foreign-import:unknown_of"]

  // Turns an Unknown back into a value of any type.
  //
  // If the return type is bogus, later code may corrupt and have undefined
  // behavior (kind of like JavaScript values disagreeing with TypeScript's type
  // annotations).
  [def-decl Unknown.unsafe_to [-> [type A] Unknown A]]
  [def-annotate Unknown.unsafe_to "foreign-import:unknown_unsafe_to"]

  // * Base library: Non-cryptographic Hash
  //
  // Like Haskell's 'Hashable', but the API looks like
  // https://doc.rust-lang.org/std/hash/trait.Hasher.html for convenience.

  // Hasher is an interface that you can write bytes or values of built-in
  // types into, and then call .finishAsI32().
  [class-decl Hasher]
  [class-struct Hasher
    [field write_bool [-> Bool ,]]
    [field write_i32 [-> I32 ,]]
    [field write_char [-> Char ,]]
    [field write_string [-> String ,]]
    [field finish_as_i32 [-> , I32]]
  ]

  // A trait for types that can be hashed with a Hasher.
  [class-decl Hash [A *]]
  [class-struct Hash
    [field on_hash_to [-> A Hasher ,]]
  ]

  [def-decl pub Hash.hash_to [-> [type A] A [auto [Hash A]] Hasher ,]]
  [def-impl Hash.hash_to [\ [type A] a [auto A.Hash] h [do
    [A.Hash . Hash.on_hash_to a h]
  ]]]


  [def-decl pub Hash.hash [-> [type A] A [auto [Hash A]] I32]]
  [def-impl Hash.hash [\ [type A] a [auto A.Hash] [do
    [panic "TODO: Implement"]
  ]]]

  [def-decl pub auto Unit.Hash [Hash ,]]
  [def-impl Unit.Hash [Hash.new 
    [\ _self h [h . Hasher.write_i32 0]]
  ]]

  [def-decl pub auto Pair.Hash [-> [type A] [type B] [auto [Hash A]] [auto [Hash B]] [Hash [; A B]]]]
  [def-impl Pair.Hash [\ [type A] [type B] [auto A.Hash] [auto B.Hash] [Hash.new
    [\ self h [do
      [self . Pair.fst . Hash.hash_to h]
      [self . Pair.snd . Hash.hash_to h]
    ]]
  ]]]

  [def-decl pub auto I32.Hash [Hash I32]]
  [def-impl I32.Hash [Hash.new 
    [\ self h [h . Hasher.write_i32 self]]
  ]]

  [def-decl pub auto Bool.Hash [Hash Bool]]
  [def-impl Bool.Hash [Hash.new 
    [\ self h [h . Hasher.write_bool self]]
  ]]

  [def-decl pub auto Char.Hash [Hash Char]]
  [def-impl Char.Hash [Hash.new 
    [\ self h [h . Hasher.write_char self]]
  ]]

  [def-decl pub auto String.Hash [Hash String]]
  [def-impl String.Hash [Hash.new 
    [\ self h [h . Hasher.write_string self]]
  ]]

  [def-decl pub auto Maybe.Hash [-> [type A] [auto [Hash A]] [Hash [Maybe A]]]]
  [def-impl Maybe.Hash [\ [type A] [auto A.Hash] [Hash.new
    [\ self h [match self
      [[None] [do
        [h . Hasher.write_i32 0]
      ]]
      [[Some a] [do
        [h . Hasher.write_i32 1]
        [a . Hash.hash_to h]
      ]]
    ]]
  ]]]

  [def-decl pub auto Either.Hash [-> [type E] [type A] [auto [Hash E]] [auto [Hash A]] [Hash [Either E A]]]]
  [def-impl Either.Hash [\ [type E] [type A] [auto E.Hash] [auto A.Hash] [Hash.new
    [\ self h [match self
      [[L a] [do
        [h . Hasher.write_i32 0]
        [a . Hash.hash_to h]
      ]]
      [[R b] [do
        [h . Hasher.write_i32 1]
        [b . Hash.hash_to h]
      ]]
    ]]
  ]]]

  // * Async monad
  //
  // Typically used to fit into JS environments, but can be implemented in other
  // environments as well.

  [class-decl HostPromise [A *]]

  // Creates a Promise that is immediately resolved with the given value.
  //
  // This function is assumed to be expensive (e.g. it may involve a microtask
  // delay in JS).
  [def-decl HostPromise.of [-> [type A] A [HostPromise A]]]
  [def-annotate HostPromise.of "foreign-import:host_promise_of"]

  [def-decl HostPromise.then [-> [type A] [type B] [HostPromise A] [-> A [HostPromise B]] [HostPromise B]]]
  [def-annotate HostPromise.then "foreign-import:host_promise_then"]

  [class-decl Async [A *]]
  [class-struct Async
    [field on_map [-> [type B] [-> A B] [Async B]]]
    [field on_to_promise [-> , [HostPromise A]]]
  ]

  [def-decl Async.map [-> [type A] [type B] [Async A] [-> A B] [Async B]]]
  [def-impl Async.map [\ [type A] [type B] m.a a->b [do
    [m.a . Async.on_map a->b]
  ]]]

  [def-decl Async.pure [-> [type A] A [Async A]]]
  [def-impl Async.pure [\ [type A] a [do
    [Async.new* [dict
      [on_map [\ [type B] a->b [Async.pure [a->b a]]]]
      [on_to_promise [\ _ [HostPromise.of a]]]
    ]]
  ]]]

  [def-decl Async.to_promise [-> [type A] [Async A] [HostPromise A]]]
  [def-impl Async.to_promise [\ [type A] m.a [m.a . Async.on_to_promise ,]]]

  [def-decl pub Async.await [-> [type A] [HostPromise A] [Async A]]]
  [def-decl pub Async.await* [-> [type A] [type B] [HostPromise A] [-> A B] [Async B]]]

  [def-impl Async.await [\ [type A] promise [do
    [Async.new* [dict
      [on_map [\ [type B] a->b [Async.await* promise a->b]]]
      [on_to_promise [\ _ promise]]
    ]]
  ]]]

  [def-impl Async.await* [\ [type A] [type B] promise a->b [do
    [Async.new* [dict
      [on_map [\ [type C] b->c [Async.await* promise [a->b . fn.| b->c]]]]
      [on_to_promise [\ _ [promise . HostPromise.then [\ a [a . a->b . HostPromise.of]]]]]
    ]]
  ]]]

  [def-decl Async.then [-> [type A] [type B] [Async A] [-> A [Async B]] [Async B]]]
  [def-impl Async.then [\ [type A] [type B] m.a a->m.b
    [Async.await [m.a . Async.to_promise . HostPromise.then [\ a [a . a->m.b . Async.to_promise]]]]
  ]]

  [def-decl Async.<*> [-> [type A] [type B] [Async [-> A B]] [Async A] [Async B]]]
  [def-impl Async.<*> [\ [type A] [type B] m.a->b m.a [do
    [m.a->b . Async.then [\ a->b [m.a . Async.then [\ a [Async.pure [a->b a]]]]]]
  ]]]

  [def-decl pub auto Async.Functor [Functor Async]]
  [def-impl Async.Functor [Functor.new 
    [\ [type A] [type B] m.a a->b [Async.map m.a a->b]]
  ]]

  [def-decl pub auto Async.Pure [Pure Async]]
  [def-impl Async.Pure [Pure.new 
    [\ [type A] a [Async.pure a]]
  ]]
  
  [def-decl pub auto Async.Apply [Apply Async]]
  [def-impl Async.Apply [Apply.new 
    [\ [type A] [type B] m.a->b m.a [Async.<*> m.a->b m.a]]
  ]]

  [def-decl pub auto Async.Then [Then Async]]
  [def-impl Async.Then [Then.new 
    [\ [type A] [type B] m.a a->m.b [Async.then m.a a->m.b]]
  ]]

  [def-decl pub Async.lazy [-> [type A] [-> , [Async A]] [Async A]]]
  [def-impl Async.lazy [\ [type A] thunk [do
    [Async.new* [dict
      [on_map [\ [type B] a->b [Async.lazy [\ _ [thunk , . Async.map a->b]]]]]
      [on_to_promise [\ _ [thunk , . Async.to_promise]]]
    ]]
  ]]]

  // * System utils

  [def-decl pub sys.println [-> String ,]]
  [def-annotate sys.println "foreign-import:sys_println"]
]
