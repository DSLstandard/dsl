[file
  [import "base.dsl"]

  // A mutable binary MIN-heap with respect to a user-provided comparison
  // function.
  //
  // TIP: If you only want to store the key value pairs and only compare the
  // keys, simply tailor 'is_<' to only look at the keys.
  [class-decl BinaryHeap [A *]]
  [class-struct BinaryHeap
    [field array [Array A]]
    [field is_< [-> A A Bool]]
  ]

  [def-decl pub BinaryHeap.size [-> [type A] [BinaryHeap A] I32]]
  [def-impl BinaryHeap.size [\ [type A] self [self . BinaryHeap.array . Array.length]]]

  [def-decl pub BinaryHeap.is_empty [-> [type A] [BinaryHeap A] Bool]]
  [def-impl BinaryHeap.is_empty [\ [type A] self
    [self . BinaryHeap.array . Array.is_empty]]]

  [def-decl child_indices_of [-> I32 [; I32 I32]]]
  [def-impl child_indices_of [\ i [do
    [let i*2 [i . I32.* 2]]
    [let l [i*2 . I32.+ 1]]
    [let r [i*2 . I32.+ 2]]
    [; l r]
  ]]]

  // Undefined behavior if index is <= 0.
  [def-decl parent_index_of [-> I32 I32]]
  [def-impl parent_index_of [\ i [[i . I32.- 1] . I32.floordiv 2]]]

  // Undefined behavior if heap is empty
  [def-decl get_last_index [-> [type A] [BinaryHeap A] I32]]
  [def-impl get_last_index [\ [type A] self [self . BinaryHeap.array . Array.length . I32.- 1]]]

  [def-decl sift_down_from [-> [type A] [BinaryHeap A] I32 ,]]
  [def-impl sift_down_from [\ [type A] self p [do
    [let array [self . BinaryHeap.array]]
    [let is_< [self . BinaryHeap.is_<]]

    [let least [var p]]
    [let= [; l r] [child_indices_of p]]

    // TODO: Optimize

    [array . Array.at? l . Maybe.when_some [\ l_val [do
      [when [is_< l_val [array . Array.at [get least]]] [\ _ [do
        [least . set l]
      ]]]
    ]]]

    [array . Array.at? r . Maybe.when_some [\ r_val [do
      [when [is_< r_val [array . Array.at [get least]]] [\ _ [do
        [least . set r]
      ]]]
    ]]]

    [let least [get least]]
    [when [p . != least] [\ _ [do
      [array . Array.swap p least]
      [sift_down_from self least]
    ]]]
  ]]]

  [def-decl sift_up_from [-> [type A] [BinaryHeap A] I32 ,]]
  [def-impl sift_up_from [\ [type A] self i [do
    [if-let [true] [i . == 0] ,]

    [let array [self . BinaryHeap.array]]
    [let is_< [self . BinaryHeap.is_<]]

    [let p [parent_index_of i]]

    [let p_val [array . Array.at p]]
    [let i_val [array . Array.at i]]

    [when [i_val . is_< p_val] [\ _ [do
      [array . Array.swap i p]
      [self . sift_up_from p]
    ]]]
  ]]]

  [def-decl pub BinaryHeap.peek? [-> [type A] [BinaryHeap A] [Maybe A]]]
  [def-impl BinaryHeap.peek? [\ [type A] self
    [self . BinaryHeap.array . Array.at? 0]
  ]]

  [def-decl pub BinaryHeap.peek [-> [type A] [BinaryHeap A] A]]
  [def-impl BinaryHeap.peek [\ [type A] self
    [self . BinaryHeap.peek? . Maybe.expect "BinaryHeap.peek: heap is empty"]
  ]]

  [def-decl pub BinaryHeap.pop? [-> [type A] [BinaryHeap A] [Maybe A]]]
  [def-impl BinaryHeap.pop? [\ [type A] self [do
    [let array [self . BinaryHeap.array]]

    [if-let [true] [array . Array.is_empty]
      [None]
    ]

    [if-let [true] [array . Array.is_one]
      [Some [array . Array.pop_last]]
    ]

    [let top_val [array . Array.at 0]]
    [array . Array.set 0 [Array.pop_last array]]
    [self . sift_down_from 0]
    [Some top_val]
  ]]]

  [def-decl pub BinaryHeap.pop [-> [type A] [BinaryHeap A] A]]
  [def-impl BinaryHeap.pop [\ [type A] self
    [self . BinaryHeap.pop? . Maybe.expect "BinaryHeap.pop: heap is empty"]
  ]]

  [def-decl pub BinaryHeap.pop_ [-> [type A] [BinaryHeap A] ,]]
  [def-impl BinaryHeap.pop_ [\ [type A] self [do
    [self . BinaryHeap.pop . unit]
  ]]]

  [def-decl pub BinaryHeap.push [-> [type A] [BinaryHeap A] A ,]]
  [def-impl BinaryHeap.push [\ [type A] self value [do
    [let array [self . BinaryHeap.array]]

    [array . Array.append value]
    [self . sift_up_from [get_last_index self]]
  ]]]

  // Like Python's 'heapq.heappushpop'. More optimized than manually doing a
  // push followed by a pop.
  //
  // Works even if the heap is empty.
  [def-decl pub BinaryHeap.push_pop [-> [type A] [BinaryHeap A] A A]]
  [def-impl BinaryHeap.push_pop [\ [type A] self value [do
    [match [self . BinaryHeap.peek?]
      [[None] [do
        value
      ]]
      [[Some tip] [do
        [self . BinaryHeap.array . Array.set 0 value]
        [self . sift_down_from 0]
        tip
      ]]
    ]
  ]]]

  // Like 'BinaryHeap.push_pop' but discards the popped value.
  [def-decl pub BinaryHeap.push_pop_ [-> [type A] [BinaryHeap A] A ,]]
  [def-impl BinaryHeap.push_pop_ [\ [type A] self value [do
    [self . BinaryHeap.push_pop value . unit]
  ]]]

  [def-decl pub BinaryHeap.push_many [-> [type A] [BinaryHeap A] [Iter A] ,]]
  [def-impl BinaryHeap.push_many [\ [type A] self values [do
    // TODO: Optimize
    [Iter.foreach values [\ value [do
      [self . BinaryHeap.push value]
      continue
    ]]]
  ]]]

  [def-decl pub BinaryHeap.clear [-> [type A] [BinaryHeap A] ,]]
  [def-impl BinaryHeap.clear [\ [type A] self [do
    [self . BinaryHeap.array . Array.clear]
  ]]]

  // O(N*log_2(N)), the BinaryHeap becomes empty after the iterator is drained.
  [def-decl pub BinaryHeap.drain_iter [-> [type A] [BinaryHeap A] [Iter A]]]
  [def-impl BinaryHeap.drain_iter [\ [type A] self [do
    [Iter.new [\ _ [self . BinaryHeap.pop?]]]
  ]]]

  // Create a binary heap from an array by taking ownership of the array and
  // heapifying it.
  [def-decl pub BinaryHeap.own_and_heapify
    [-> [type A] [Array A] [-> A A Bool] [BinaryHeap A]]]
  [def-impl BinaryHeap.own_and_heapify [\ [type A] array is_< [do
    [let self [BinaryHeap.new array is_<]]
    [let N [array . Array.length]]

    [loop<n_reversed [N . I32.floordiv 2] [\ p [do
      [self . sift_down_from p]
      continue
    ]]]

    self
  ]]]

  [def-decl pub BinaryHeap.create [-> [type A] [-> A A Bool] [BinaryHeap A]]]
  [def-impl BinaryHeap.create [\ [type A] is_< [do
    [let array [Array.create ,]]
    [BinaryHeap.new array is_<]
  ]]]
]