[file
  [import "base.dsl"]
  [import "wlp.dsl"]

  [tydef JSExpr * [Doc Void]]

  [def-decl pub JSExpr.inline [-> String JSExpr]]
  [def-impl JSExpr.inline [\ code [do
    [Doc.of code]
  ]]]

  [def-decl pub JSExpr.ident [-> String JSExpr]]
  [def-impl JSExpr.ident [\ ident [do
    // TODO: Panic on bad identifier
    [JSExpr.inline ident]
  ]]]

  [def-decl pub JSExpr.bool [-> Bool JSExpr]]
  [def-impl JSExpr.bool [\ b [do
    [JSExpr.inline [if b "true" "false"]]
  ]]]

  [def-decl pub JSExpr.i32 [-> I32 JSExpr]]
  [def-impl JSExpr.i32 [\ x [do
    [x . I32.to_str . JSExpr.inline]
  ]]]

  [def-decl pub JSExpr.int [-> Int JSExpr]]
  [def-impl JSExpr.int [\ x [do
    [x . Int.to_str . JSExpr.inline]
  ]]]

  [def-decl pub JSExpr.string [-> String JSExpr]]
  [def-impl JSExpr.string [\ str [do
    [str . String.stringify_as_json . JSExpr.inline]
  ]]]

  [def-decl pub JSExpr.return [-> JSExpr JSExpr]]
  [def-impl JSExpr.return [\ value [do
    [Foldable.mconcat [vec
      [JSExpr.inline "return "]
      value
      [JSExpr.inline ","]
    ]]
  ]]]

  [def-decl pub JSExpr.block [-> JSExpr JSExpr JSExpr JSExpr]]
  [def-impl JSExpr.block [\ begin end body [do
    // TODO: Should be implemented as:
    // ```
    // P.flatAlt
    //   ( P.vcat
    //       [ begin
    //       , P.indent indent body
    //       , end
    //       ]
    //   )
    //   ( P.hcat [begin, body, end]
    //   )
    // ```
    [Foldable.mconcat [vec
      begin
      [JSExpr.inline " "]
      body
      [JSExpr.inline " "]
      end
    ]]
  ]]]

  [def-decl pub JSExpr.arrow [-> [Iter JSExpr] JSExpr JSExpr]]
  [def-impl JSExpr.arrow [\ params [
    JSExpr.block
      [Foldable.mconcat [vec [Doc.tupled params] [JSExpr.inline " => {"]]]
      [JSExpr.inline "}"]
  ]]]

  [def-decl pub JSExpr.call [-> JSExpr [Iter JSExpr] JSExpr]]
  [def-impl JSExpr.call [\ callee args
    [callee . Doc.<> [args . Doc.tupled]]
  ]]

  [def-decl pub JSExpr.call_method [-> JSExpr String [Iter JSExpr] JSExpr]]
  [def-impl JSExpr.call_method [\ obj method_name args
    [Foldable.mconcat [vec
      obj
      [JSExpr.inline "."]
      [JSExpr.ident method_name]
      [args . Doc.tupled]
    ]
  ]]]

  // Immediately-invoked function expression
  [def-decl pub JSExpr.iife [-> JSExpr JSExpr]]
  [def-impl JSExpr.iife [\ body [do
    [JSExpr.block [JSExpr.inline "(() => {"] [JSExpr.inline "})()"] body]
  ]]]

  [def-decl pub JSExpr.conditional_ternary [-> JSExpr JSExpr JSExpr JSExpr]]
  [def-impl JSExpr.conditional_ternary [\ cond on_true on_false
    [Foldable.mconcat [vec
      cond
      [JSExpr.inline " ? "]
      on_true
      [JSExpr.inline " : "]
      on_false
    ]]
  ]]

  [def-decl pub JSExpr.if_else [-> JSExpr JSExpr JSExpr JSExpr]]
  [def-impl JSExpr.if_else [\ cond on_true on_false
    [Doc.vcat [Vec.iter [vec
      [JSExpr.block
        [Foldable.mconcat [vec [JSExpr.inline "if ("] cond [JSExpr.inline ") {"]]]
        [JSExpr.inline "}"]
        on_true
      ]
      [JSExpr.block
        [JSExpr.inline "else {"]
        [JSExpr.inline "}"]
        on_false
      ]
    ]]]
  ]]

  [def-decl pub JSExpr.object [-> [Iter [; String JSExpr]] JSExpr]]
  [def-impl JSExpr.object [\ fields [do
    [Doc.setted [fields . Iter.map [\ field [do
      [let= [; key value] field]
      [Foldable.mconcat [vec
        [JSExpr.string key]
        [JSExpr.inline ": "]
        value
      ]]
    ]]]]
  ]]]

  [def-decl pub JSExpr.arrow [-> [Iter String] JSExpr JSExpr]]
  [def-impl JSExpr.arrow [\ params body
    [Foldable.mconcat [vec
      [Doc.tupled [params . Iter.map JSExpr.ident]]
      [JSExpr.inline " => "]
      body
    ]]
  ]]

  [def-decl pub JSExpr.function [-> [Iter String] JSExpr JSExpr]]
  [def-impl JSExpr.function [\ params
    [JSExpr.block
      [params . Iter.map JSExpr.ident . Doc.tupled . Doc.<> [JSExpr.inline " => {"]]
      [JSExpr.inline "}"]
    ]
  ]]

  [def-decl pub JSExpr.return [-> JSExpr JSExpr]]
  [def-impl JSExpr.return [\ value [do
    [JSExpr.inline "return " . <> value]
  ]]]

  [def-decl pub JSExpr.array [-> [Iter JSExpr] JSExpr]]
  [def-impl JSExpr.array [\ elems [do
    [elems . Doc.listed]
  ]]]

  [def-decl pub JSExpr.index [-> JSExpr JSExpr JSExpr]]
  [def-impl JSExpr.index [\ array index
    [array . Doc.<> [index . Doc.brackets]]
  ]]

  [def-decl pub JSExpr.case [-> JSExpr JSExpr JSExpr]]
  [def-impl JSExpr.case [\ pattern body [do
    [Foldable.mconcat [vec
      [JSExpr.inline "case "]
      pattern
      [JSExpr.inline ":"]
      body
    ]]
  ]]]

  [def-decl pub JSExpr.case_default [-> JSExpr JSExpr]]
  [def-impl JSExpr.case_default [\ body [do
    [Foldable.mconcat [vec
      [JSExpr.inline "default: "]
      body
    ]]
  ]]]

  [def-decl pub JSExpr.switch [-> JSExpr [Iter JSExpr] JSExpr]]
  [def-impl JSExpr.switch [\ scrutinee cases [do
    [JSExpr.block
      [Foldable.mconcat [vec [JSExpr.inline "switch ("] scrutinee [JSExpr.inline ") {"]]]
      [JSExpr.inline "}"]
      [cases . Doc.vcat]
    ]
  ]]]

  [def-decl pub JSExpr.const_assign [-> String JSExpr JSExpr]]
  [def-impl JSExpr.const_assign [\ var_name value [do
    [Foldable.mconcat [vec
      [JSExpr.inline "const "]
      [JSExpr.ident var_name]
      [JSExpr.inline " = "]
      value
      [JSExpr.inline ";"]
    ]]
  ]]]


  [def-decl pub JSExpr.var_assign [-> String JSExpr JSExpr]]
  [def-impl JSExpr.var_assign [\ var_name value [do
    [Foldable.mconcat [vec
      [JSExpr.inline "var "]
      [JSExpr.ident var_name]
      [JSExpr.inline " = "]
      value
      [JSExpr.inline ";"]
    ]]
  ]]]
]
