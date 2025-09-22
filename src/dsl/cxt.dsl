[file
  [import "base.dsl"]
  [import "colog.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/eval_cxt.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/location.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/namespace.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "wlp.dsl"]

  // * ExprError

  [class-decl ExprError]
  [class-struct ExprError
    // The associated expression
    [field expr Expr]
    // Description
    [field desc [Doc Void]]
  ]

  // * Cxt

  [class-decl Cxt]
  [class-struct Cxt
    [field db Database]
    [field metas MetaStore]
    [field debugging Bool]
    [field debug_out [LogAction [Doc Void]]]

    // * Type scope
    [field ty_scope TyScope]
    // * Term scope
    [field tm_scope TmScope]
    // A list of the bound auto arguments in the current scope. Used for instance resolution.
    [field tm_bound_autos [List Lvl]]

    // * Namespace and exports
    [field namespace [Namespace UID]]
    [field exports [Namespace UID]]

    // * Error reporting
    [field expr_errors [Array ExprError]]
  ]

  [class-decl Cxt.create.Params]
  [class-struct Cxt.create.Params
    [field debugging Bool]
    [field debug_out [LogAction [Doc Void]]]
  ]

  [def-decl pub Cxt.eval [-> Cxt EvalCxt]]
  [def-impl Cxt.eval [\ cxt [do
    [EvalCxt.new* [dict
      [db [cxt . Cxt.db]]
      [metas [cxt . Cxt.metas]]
    ]]
  ]]]

  // Error reporting

  [def-decl pub Cxt.mark_expr_error [-> Cxt Expr [Doc Void] ,]]
  [def-impl Cxt.mark_expr_error [\ cxt expr msg [do
    [cxt . Cxt.expr_errors . Array.append [ExprError.new expr msg]]
  ]]]

  // Scope utils

  [def-decl pub Cxt.extend_ty [-> Cxt TyScope Cxt]]
  [def-impl Cxt.extend_ty [\ cxt sub [do
    [cxt . Cxt.=ty_scope [cxt . Cxt.ty_scope . Scope.extend sub]]
  ]]]

  [def-decl pub Cxt.extend_tm [-> Cxt TmScope Cxt]]
  [def-impl Cxt.extend_tm [\ cxt sub [do
    [cxt . Cxt.=tm_scope [cxt . Cxt.tm_scope . Scope.extend sub]]
  ]]]

  [def-decl pub Cxt.extend_tm_bound_auto [-> Cxt Lvl Cxt]]
  [def-impl Cxt.extend_tm_bound_auto [\ cxt l [do
    [cxt . Cxt.=tm_bound_autos [cxt . Cxt.tm_bound_autos . List.cons l]]
  ]]]

  // Namespace utils

  [def-decl pub Cxt.open_name [-> Cxt Name UID ,]]
  [def-impl Cxt.open_name [\ cxt name uid [do
    [cxt . Cxt.namespace . Namespace.set name uid]
  ]]]

  [def-decl pub Cxt.export_name [-> Cxt Name UID ,]]
  [def-impl Cxt.export_name [\ cxt name uid [do
    [cxt . Cxt.exports . Namespace.set name uid]
  ]]]

  [def-decl pub Cxt.lookup_namespace [-> Cxt Name [Maybe UID]]]
  [def-impl Cxt.lookup_namespace [\ cxt name [do
    [cxt . Cxt.namespace . Namespace.get? name]
  ]]]
]
