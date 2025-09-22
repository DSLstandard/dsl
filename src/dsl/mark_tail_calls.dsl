[file
  [import "base.dsl"]
  [import "dsl/types.dsl"]

  // Mark tail calls in a term by marking 'TmCall's in tail position with
  // 'IsTail True'.
  //
  // This function don't bother optimizing tail calls inside lambdas at tail
  // position. It is the responsibility of other parts of the compiler to ensure
  // these are already TCO-ed beforehand.
  [def-decl pub mark_tail_calls [-> Tm Tm]]
  [def-impl mark_tail_calls [\ in_tm [match in_tm
    // Mark tail call
    [[App [; _ f x]] [Tm.App true f x]]

    // Go closer to tail calls
    [[Let [; vals next]]
      [Tm.Let vals [mark_tail_calls next]]]
    [[If [; cond on_true on_false]]
      [Tm.If cond [mark_tail_calls on_true] [mark_tail_calls on_false]]]
    [[Match tree] [do
      [let clauses* [Array.create ,]]
      [tree . TmMatch.Tree.clauses . Array.foreach [\ clause [do
        [let body_tm* [clause . TmMatch.Clause.body_tm . mark_tail_calls]]
        [let clause* [clause . TmMatch.Clause.=body_tm body_tm*]]
        [clauses* . Array.append clause*]
        continue
      ]]]

      [let tree* [tree . TmMatch.Tree.=clauses clauses*]]
      [Tm.Match tree*]
    ]]

    // See comment of this function. Do not recurse into lambdas.
    [[Lam body] in_tm]

    // Uninteresting cases
    [[Error] in_tm]
    [[Var] in_tm]
    [[Def] in_tm]
    [[Lit] in_tm]
    [[Class] in_tm]
    [[Tuple] in_tm]
    [[Vec] in_tm]
    [[Dict] in_tm]
    [[GetDictKey] in_tm]
    [[GetTupleIndex] in_tm]
    [[SetTupleIndex] in_tm]
  ]]]
]
