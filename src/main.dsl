[file
  [import "base.dsl"]
  [import "colog.dsl"]
  [import "dsl/cxt.dsl"]
  [import "dsl/pretty_ty.dsl"]
  [import "dsl/expr.dsl"]
  [import "dsl/eval_cxt.dsl"]
  [import "dsl/eval_ty.dsl"]
  [import "dsl/quote_ty.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/parser.dsl"]
  [import "dsl/location.dsl"]
  [import "dsl/process_file.dsl"]
  [import "dsl/js/jsexpr.dsl"]
  [import "free.dsl"]
  [import "fs.dsl"]
  [import "astar_monad.dsl"]
  [import "dsl/js/codegen.dsl"]
  [import "binary_heap.dsl"]
  [import "wlp.dsl"]

  [def-decl main [-> , ,]]
  [def-annotate main "foreign-export:dsl_main"]
  [def-impl main [\ _ [do
    [sys.println ">>> Program start"]

    [let dsl [DSL.create [dict
      [debugging false]
      [debug_out [LogAction.println . LogAction.cmap [Doc.to_str]]]
      [resolve_import_string [\ import_string [do
        [let path ["./src/" . <> import_string]]
        [Some [DSLRes.new* [dict
          [dim path]
          [on_read_source [\ _ [do
            [sys.println ["[!] Read import: ". <> path]]
            [let source [fs.read_file_utf8 path
              . Either.expect* [\ _ ["Failed to read import: " . <> path]]]]
            source
          ]]]
        ]]]
      ]]]
    ]]]

    [dsl . DSL.load_import "main.dsl" . unit]

    [let js_expr [codegen_js [codegen_js.Params.new* [dict
      [db [dsl . DSL.db]]
      [import_module "./dsl_foreign"]
    ]]]]

    [let out_path "./generated.js"]
    [fs.write_file_utf8 out_path [js_expr . Doc.to_str]
      . Either.expect* [\ _ ["Failed to write output file: " . <> out_path]]]
    [sys.println ["Written output to " . <> out_path]]

    [sys.println "<<< Pregram end"]
  ]]]
]
