[file
  [import "base.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/database.dsl"]
  [import "wlp.dsl"]

  // Returns all distinct defs (by their UID) used by the Tm. Does not recurse
  // into defs.
  [def-decl pub Tm.scan_defs [-> Tm I32MutSet]]
  [def-impl Tm.scan_defs [\ in_tm [do
    [let found_uids [I32MutSet.create ,]]

    [let record_uid [\ [uid UID] [do
      [found_uids . I32MutSet.add uid]
    ]]]

    [let* [go [\ [tm Tm] [match tm
      [[Var _] ,]
      [[Def uid] [record_uid uid]]
      [[Lam body] [go body]]
      [[App [; _ f x]] [do
        [go f]
        [go x]
      ]]
      [[SetTupleIndex [; subj _ value]] [do
        [go subj]
        [go value]
      ]]
      [[GetTupleIndex [; subj _]] [go subj]]
      [[GetDictKey [; subj _]] [go subj]]
      [[Let [; vals body]] [do
        [vals . Array.foreach [\ val [do
          [go val]
          continue
        ]]]
        [go body]
      ]]
      [[Lit _] ,]
      [[Tuple elems] [do
        [elems . Array.foreach [\ elem [do
          [go elem]
          continue
        ]]]
      ]]
      [[Vec elems] [do
        [elems . Array.foreach [\ elem [do
          [go elem]
          continue
        ]]]
      ]]
      [[Class [; _class_uid _member_id value]] [do
        [go value]
      ]]
      [[Dict entries] [do
        [entries . Array.foreach [\ entry [do
          [let= [; key value] entry]
          [go value]
          continue
        ]]]
      ]]
      [[Match tree] [do
        [go [tree . TmMatch.Tree.root_scrut_tm]]
        [tree . TmMatch.Tree.clauses . Array.foreach [\ clause [do
          [clause . TmMatch.Clause.body_tm . go]
          [clause . TmMatch.Clause.guard_tm . Maybe.when_some go]
          continue
        ]]]
      ]]
      [[If [; cond on_true on_false]] [do
        [go cond]
        [go on_true]
        [go on_false]
      ]]
      [[Error] ,]
    ]]]]
    [go in_tm]

    found_uids
  ]]]
]