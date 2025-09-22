[file
  [import "base.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/eval_cxt.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]

  [def-decl pub quote_ty [-> EvalCxt TyLvl Vy Ty]]
  [def-impl quote_ty [\ cxt in_lvl in_ty [do
    [let* [go [\ [lvl TyLvl] [ty Vy] [do
      [match [force_ty_weak cxt ty]
        [[Rigid [; h sp]] [do
          [let f [match h
            [[Bound l] [do
              [let i [l . lvl_to_ix lvl]]

              // A very common error during compiler development, worth a
              // dedicated check.
              [when [i . < 0] [\ _ [do
                [panic ["quote_ty: bad bound variable: "
                  . <> [I32.to_str i]
                  . <> " (lvl = " . <> [I32.to_str lvl]
                  . <> ", l = " . <> [I32.to_str l]
                  . <> ")"]]
              ]]]

              [Ty.Var i]
            ]]
            [[Known k] [Ty.Known k]]
            [[Class u] [Ty.Class u]]
          ]]
          [let sp [sp . Spine.map [\ x [go lvl x]]]]
          [Ty.app_spine f sp]
        ]]
        [[Flex [; m sp]] [do
          [let sp [sp . Spine.map [\ x [go lvl x]]]]
          [Ty.app_spine [Ty.Meta m] sp]
        ]]
        [[Tuple elems] [do
          [Ty.Tuple [elems . Array.map [go lvl]]]
        ]]
        [[Dict d] [do
          [Ty.Dict [d . StringMap.map [go lvl]]]
        ]]
        [[Arr [; icit dom cod]] [do
          [Ty.Arr icit [go lvl dom] [go lvl cod]]
        ]]
        [[Lam [; kind c]] [do
          [let e [eval_ty_closure cxt c [Vy.bound lvl]]]
          [let e [go [I32.suc lvl] e]]
          [Ty.Lam kind e]
        ]]
        [[Forall [; kind c]] [do
          [let e [eval_ty_closure cxt c [Vy.bound lvl]]]
          [let e [go [I32.suc lvl] e]]
          [Ty.Forall kind e]
        ]]
      ]
    ]]]]
    [go in_lvl in_ty]
  ]]]
]