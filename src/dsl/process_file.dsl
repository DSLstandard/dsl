[file
  [import "base.dsl"]
  [import "colog.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/location.dsl"]
  [import "dsl/meta_store.dsl"]
  [import "dsl/namespace.dsl"]
  [import "dsl/parser.dsl"]
  [import "dsl/pexpr.dsl"]
  [import "dsl/pexpr_utils.dsl"]
  [import "dsl/process_class.dsl"]
  [import "dsl/process_def.dsl"]
  [import "dsl/process_tydef.dsl"]
  [import "dsl/types.dsl"]
  [import "wlp.dsl"]

  [class-decl DSLRes]
  [class-struct DSLRes
    // The resolved 'Dim' of the imported resource. Multiple import strings
    // could point to the same Dim, and Dim is the absolute identifier for the
    // resource.
    [field dim Dim]

    // Read the resource's source. This function may read from the file system
    // or fetch the data from the network with a cache, etc.
    //
    // TODO: Use a promise
    [field on_read_source [-> , String]]
  ]

  [class-decl DSL]
  [class-struct DSL
    [field db Database]
    [field metas MetaStore]
    [field debugging Bool]
    [field debug_out [LogAction [Doc Void]]]

    // Given an import string, resolve it to a DSLRes if possible.
    [field resolve_import_string [-> String [Maybe DSLRes]]]

    // Map of processed resource Dims to their exports.
    [field dim_to_exports [StringMutMap [Namespace UID]]]
  ]

  [def-decl pub DSL.create [->
    [Dict
      [debugging Bool]
      [debug_out [LogAction [Doc Void]]]
      [resolve_import_string [-> String [Maybe DSLRes]]]
    ]
    DSL
  ]]
  [def-impl DSL.create [\ params [do
    [let db [Database.create ,]]
    [let metas [MetaStore.create ,]]
    [DSL.new* [dict
      [db db]
      [metas metas]
      [debugging [params ./ debugging]]
      [debug_out [params ./ debug_out]]

      [resolve_import_string [params ./ resolve_import_string]]
      [dim_to_exports [StringMutMap.create ,]]
    ]]
  ]]]

  [def-decl pat_import [PExpr [-> DSL Cxt ,]]]
  [def-decl pat_decl [PExpr [-> DSL Cxt ,]]]
  [def-decl pat_file [PExpr [Array Expr]]]
  [def-decl pub DSL.load_import [-> DSL String [Namespace UID]]]

  [def-impl pat_import [PExpr.block "import" [
    [\ [import_string String] [block PExpr.Block] [dsl DSL] [cxt Cxt] [do
      [let ns [dsl . DSL.load_import import_string]]
      [cxt . Cxt.namespace . Namespace.include ns]
    ]]
    . <$> [PList.one_of [PExpr.label "<import-string>" PExpr.any_str]]
  ]]]

  [def-impl pat_decl [do
    [let ignore_dsl [\ [f [-> Cxt ,]] [_dsl DSL] [cxt Cxt] [f cxt]]]
    [Foldable.mconcat [vec
      pat_import
      [pat_class_decl . map ignore_dsl]
      [pat_class_enum . map ignore_dsl]
      [pat_class_struct . map ignore_dsl]
      [pat_def_annotate . map ignore_dsl]
      [pat_def_decl . map ignore_dsl]
      [pat_def_impl . map ignore_dsl]
      [pat_ty_def_decl . map ignore_dsl]
    ]]]
  ]

  [def-impl pat_file
    [PExpr.block_ "file"
      [PList.many_of [PExpr.label "<decl>" PExpr.any_expr]]
    ]
  ]

  [def-impl DSL.load_import [\ self import_string [do
    [let res [self . DSL.resolve_import_string import_string]]

    [else-let [Some res] res [do
      [TODO "import_by_string: Handle import failure"]
    ]]

    [if-let [Some exports] [self . DSL.dim_to_exports . StringMutMap.get? [res . DSLRes.dim]] [do
      // Dim was already processed
      exports
    ]]

    [let cxt [Cxt.new* [dict
      [db [self . DSL.db]]
      [metas [self . DSL.metas]]
      [debugging [self . DSL.debugging]]
      [debug_out [self . DSL.debug_out]]
      [ty_scope [Scope.nil]]
      [tm_scope [Scope.nil]]
      [tm_bound_autos [List.nil]]
      [namespace [Namespace.create ,]]
      [exports [Namespace.create ,]]
      [expr_errors [Array.create ,]]
    ]]]

    [let source [res . DSLRes.on_read_source ,]]
    [let parse_result [DSL.parse_source [res . DSLRes.dim] source]]
    [let file_expr [parse_result . DSL.parse_source.Result.expr]]

    // Process all decls
    [let decls [match_or_report cxt file_expr pat_file . Maybe.unwrap_or* [\ _ [Array.create ,]]]]
    [decls . Array.foreach [\ decl_expr [do
      [let= [Some elab_decl] [match_or_report cxt decl_expr pat_decl]
        [_ continue]]
      [elab_decl self cxt]
      continue
    ]]]

    // Report errors
    [let syntax_errors [parse_result . DSL.parse_source.Result.syntax_errors]]
    [unless [syntax_errors . Array.is_empty] [\ _ [do
      [sys.println [
        "+++ Syntax Errors ("
        . <> [I32.to_str [syntax_errors . Array.length]]
        . <> ")"
      ]]
      [sys.println "--- Syntax Errors"]
    ]]]

    [let expr_errors [cxt . Cxt.expr_errors]]
    [unless [expr_errors . Array.is_empty] [\ _ [do
      [sys.println [
        "+++ Elab errors ("
        . <> [I32.to_str [expr_errors . Array.length]]
        . <> ")"
      ]]
      [expr_errors . Array.foreach [\ item [do
        [let msg [[item . ExprError.desc] . Doc.<+> [item . ExprError.expr . Expr.loc . Doc.of]]]
        [sys.println [msg . Doc.to_str]]
        continue
      ]]]
      [sys.println "--- Elab errors"]
    ]]]

    // Register file exports
    [let exports [cxt . Cxt.exports]]
    [self . DSL.dim_to_exports . StringMutMap.set [res . DSLRes.dim] exports]
    exports
  ]]]
]