[file
  [import "base.dsl"]
  [import "dsl/database.dsl"]
  [import "dsl/js/jsexpr.dsl"]
  [import "dsl/scan_tm_defs.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]
  [import "wlp.dsl"]

  [class-decl DefInfo]
  [class-struct DefInfo
    [field uid UID]
    [field js_name String]
    [field foreign_export [Maybe String]]
    [field foreign_import [Maybe String]]
    [field is_lazily_initialized Bool]
  ]

  [class-decl JSCodegen]
  [class-struct JSCodegen
    [field db Database]
    [field var_env [Env String]]
    [field on_fresh_var [-> , String]]

    // Def UID -> DefInfo
    [field def_info_map [I32MutMap DefInfo]]
  ]

  [def-decl JSCodegen.create [-> Database JSCodegen]]
  [def-impl JSCodegen.create [\ db [do
    [let var_counter [var 0]]
    [JSCodegen.new* [dict
      [db db]
      [var_env [Env.nil]]
      [on_fresh_var [\ _ [do
        [let i [get var_counter]]
        [set var_counter [I32.suc i]]
        ["v" . <> [I32.to_str i]]
      ]]]

      [def_info_map [I32MutMap.create ,]]
    ]]
  ]]]

  [def-decl JSCodegen.fresh_var [-> JSCodegen String]]
  [def-impl JSCodegen.fresh_var [\ g [do
    [g . JSCodegen.on_fresh_var ,]
  ]]]

  [def-decl JSCodegen.fresh_var_env [-> JSCodegen I32 [Env String]]]
  [def-impl JSCodegen.fresh_var_env [\ g num_vars [do
    [let vars [Array.create ,]]
    [loop<n num_vars [\ _ [do
      [vars . Array.append [g . JSCodegen.fresh_var]]
      continue
    ]]]
    [Env.from_iter [vars . Array.iter_reversed]]
  ]]]

  [def-decl JSCodegen.extend_var_env [-> JSCodegen [Env String] JSCodegen]]
  [def-impl JSCodegen.extend_var_env [\ g sub [do
    [g . JSCodegen.=var_env [g . JSCodegen.var_env . Env.extend sub]]
  ]]]

  [def-decl JSCodegen.get_def_js_name [-> JSCodegen UID String]]
  [def-impl JSCodegen.get_def_js_name [\ g uid [do
    [g . JSCodegen.def_info_map . I32MutMap.get uid . DefInfo.js_name]
  ]]]


  [def-decl pub prelude_jscode String]
  [def-impl prelude_jscode ["
    export class TmError extends Error {
      constructor(message = undefined) {
        super(message);
        this.name = \"TmError\";
      }
    }

    export class InexhaustiveMatchError extends Error {
      constructor(message = undefined) {
        super(message);
        this.name = \"InexhaustiveMatchError\";
      }
    }

    class TailCall {
      constructor(fn, arg) {
        this.fn = fn;
        this.arg = arg;
      }
    }

    export function $tailcall(fn, arg) {
      return new TailCall(fn, arg);
    }

    export function $apply1(fn, arg) {
      let result = fn(arg);
      while (true) {
        if (result instanceof TailCall) {
          result = result.fn(result.arg);
        } else {
          return result;
        }
      }
    }

    function $create_constant_def(initialize_constant) {
      let initialized = false;
      let cached = undefined;
      return () => {
        if (!initialized) {
          cached = initialize_constant();
          initialized = true;
        }
        return cached;
      }
    }

    function $throw_inexhaustive_match() {
      throw new InexhaustiveMatchError();
    }
  " . String.dedent . String.trim]]

  [def-decl js_safe_switch [-> JSExpr [Iter JSExpr] JSExpr]]
  [def-impl js_safe_switch [\ scrutinee cases [do
    [JSExpr.switch scrutinee [Iter.chain
      cases
      [Iter.one [JSExpr.case_default [JSExpr.inline "$throw_inexhaustive_match()"]]]
    ]]
  ]]]

  // Compiles a 'Tm' into a JS value expression.
  [def-decl compile_tm [-> JSCodegen Tm JSExpr]]

  [def-decl compile_tm.let [-> JSCodegen Tm JSExpr]]
  [def-impl compile_tm.let [\ g in_tm [do
    [let output_lines [Array.create ,]]
    [let add_line [\ [line JSExpr] [output_lines . Array.append line]]]

    [let current* [var in_tm]]
    [let let_g* [var g]]

    [loop_some [\ _ [match [get current*]
      [[Let [; vals next_tm]] [do
        [set current* next_tm]

        [let num_vals [vals . Array.length]]
        [let let_vars [Range.to num_vals
          . Range.iter
          . Iter.map [\ _ [g . JSCodegen.fresh_var]]
          . Array.from_iter]]

        [let let_env [Env.from_iter [let_vars . Array.iter_reversed]]]
        [let let_g [get let_g* . JSCodegen.extend_var_env let_env]]
        [set let_g* let_g]

        // FIXME: If the let recursive bindings have mutually recursively
        // defined constants, this breaks.
        [Iter.zip [Array.iter let_vars] [Array.iter vals] . Iter.foreach [\ input [do
          [let= [; let_var val] input]
          [let val [compile_tm let_g val]]
          [add_line [JSExpr.var_assign let_var val]]
          continue
        ]]]

        [None]
      ]]
      [last_tm [do
        [let let_g [get let_g*]]
        [let last_tm [compile_tm let_g last_tm]]

        [let body [Doc.vcat [Foldable.mconcat [vec
          [Array.iter output_lines]
          [Iter.one [JSExpr.return last_tm]]
        ]]]]

        [Some [JSExpr.iife body]]
      ]]
    ]]]
  ]]]

  [def-decl compile_tm.match [-> JSCodegen TmMatch.Tree JSExpr]]
  [def-impl compile_tm.match [\ g tree [do
    [let lines_before_switch [Array.create [type JSExpr] ,]]

    // Clause ID -> Identifier to the clause's body/guard function
    [let cid_to_body_var [I32MutMap.create [type String] ,]]
    [let cid_to_guard_var [I32MutMap.create [type String] ,]]

    [tree . TmMatch.Tree.clauses . Array.foreach [\ clause [do
      // * Setup variable environment for the clause's body / guard
      [let pat_params [Array.create ,]]
      [loop<n [clause . TmMatch.Clause.pat_env_lvl] [\ _ [do
        [pat_params . Array.append [g . JSCodegen.fresh_var]]
        continue
      ]]]
      [let pat_env [Env.from_iter [pat_params . Array.iter_reversed]]]
      [let pat_g [g . JSCodegen.extend_var_env pat_env]]

      // * Generate JS code for clause body
      [let body_var [g . JSCodegen.fresh_var]]
      [cid_to_body_var . I32MutMap.set [clause . TmMatch.Clause.id] body_var]

      [let body_jsexpr [compile_tm pat_g [clause . TmMatch.Clause.body_tm]]]
      [lines_before_switch . Array.append
        [JSExpr.const_assign body_var [JSExpr.arrow [Array.iter pat_params] body_jsexpr]]]

      // * Generate JS code for clause guard if it exists.
      [clause . TmMatch.Clause.guard_tm . Maybe.when_some [\ guard_tm [do
        [let guard_var [g . JSCodegen.fresh_var]]
        [cid_to_guard_var . I32MutMap.set [clause . TmMatch.Clause.id] guard_var]

        [let guard_jsexpr [compile_tm pat_g guard_tm]]
        [lines_before_switch . Array.append
          [JSExpr.const_assign guard_var [JSExpr.arrow [Array.iter pat_params] guard_jsexpr]]]
      ]]]

      continue
    ]]]

    // * Generate the switch statement of the tm match
    [let root_var_lvl [Env.lvl [g . JSCodegen.var_env]]] // For later pat ix/lvl calculations
    [let pat_lvl_to_ix [\ [g JSCodegen] [l PatLvl] [do
      [let pat_lvl [Env.lvl [g . JSCodegen.var_env] . I32.- root_var_lvl]]
      [l . lvl_to_ix pat_lvl]
    ]]]
    [let pat_lvl_to_jsvar [\ [g JSCodegen] [l PatLvl] [do
      [g . JSCodegen.var_env . Env.at [pat_lvl_to_ix g l]]
    ]]]

    // Recursively visits a TmMatch.Node returns a block of JS code (i.e., NOT a
    // JS value expression) that when run in JS would do 'return <value>' with
    // the result of the match clause this node leads to.
    [let* [visit_match_node [\ [g JSCodegen] [node TmMatch.Node] [match node
      [[Unhandled] [do
        [JSExpr.inline "$throw_inexhaustive_match()"]
      ]]
      [[Leaf leaf] [do
        // The clause this leaf destines to.
        [let clause_id [leaf . TmMatch.Leaf.target_clause_id]]
        [let clause [tree . TmMatch.Tree.clauses . Array.at clause_id]]

        // Setup 'js_pat_call', which collects all the pat bounds the clause has
        // and generates a JS call expression that calls the clause's body/guard
        // function with those pat bounds as arguments.
        [let js_pat_args [leaf
          . TmMatch.Leaf.pat_env
          . Env.to_spine
          . Spine.iter
          . Iter.map [\ pat_bound_l [pat_lvl_to_jsvar g pat_bound_l . JSExpr.ident]]
          . Array.from_iter
        ]]
        [let js_pat_call [\ func [JSExpr.call [JSExpr.ident func] [Array.iter js_pat_args]]]]

        [if [clause . TmMatch.Clause.has_guard]
          [do
            [let body_var [cid_to_body_var . I32MutMap.get clause_id]]
            [let guard_var [cid_to_guard_var . I32MutMap.get clause_id]]

            [let on_fail [visit_match_node g [leaf . TmMatch.Leaf.on_guard_fail
              . Maybe.expect "on_guard_tail should be defined if clause .has_guard is true"]]]
            [JSExpr.if_else [js_pat_call guard_var]
              [JSExpr.return [js_pat_call body_var]]
              [on_fail]
            ]
          ]
          [do
            [let body_var [cid_to_body_var . I32MutMap.get clause_id]]
            [JSExpr.return [js_pat_call body_var]]
          ]
        ]
      ]]
      [[SplitOn [; target_bound_l split]] [do
        // The JS var to switch on
        [let js_scrut [JSExpr.ident [pat_lvl_to_jsvar g target_bound_l]]]

        [match split
          [[Tuple branch] [do
            [let num_fields [branch . TmMatch.Split.Tuple.Branch.num_fields]]
              
            [let next_env [g . JSCodegen.fresh_var_env num_fields]]
            [let js_next [visit_match_node
              [g . JSCodegen.extend_var_env next_env]
              [branch . TmMatch.Split.Tuple.Branch.next_node]]]

            [let js_lines [Array.create ,]]
            [next_env . Env.to_spine . Spine.iter . Iter.ixed . Iter.foreach [\ input [do
              [let= [; i pat_var] input]
              [let js_tuple_elem [JSExpr.index js_scrut [JSExpr.i32 i]]]
              [js_lines . Array.append [JSExpr.const_assign pat_var js_tuple_elem]]
              continue
            ]]]
            [js_lines . Array.append js_next]

            [Doc.vcat [Array.iter js_lines]]
          ]]
          [[Class [; _class_uid branches]] [do
            [let scrut_member_id [js_scrut . JSExpr.index [JSExpr.i32 0]]]
            [let scrut_value [js_scrut . JSExpr.index [JSExpr.i32 1]]]

            [let cases [branches
              . Array.iter
              . Iter.map [\ branch [do
                [let value_var [g . JSCodegen.fresh_var]]
                [let js_next [visit_match_node
                  [g . JSCodegen.extend_var_env [Env.one value_var]]
                  [branch . TmMatch.Split.Class.Branch.next_node]]]

                [JSExpr.case
                  [JSExpr.i32 [branch . TmMatch.Split.Class.Branch.member_id]]
                  [Doc.braces [Doc.vcat [Iter.two
                    [JSExpr.const_assign value_var scrut_value]
                    js_next
                  ]]]
                ]
              ]]
            ]]
            [js_safe_switch scrut_member_id cases]
          ]]
          [[Bool branches] [do
            [let cases [branches
              . Array.iter
              . Iter.map [\ branch [do
                [let js_next [visit_match_node g [branch . TmMatch.Split.Bool.Branch.next_node]]]

                [JSExpr.case
                  [JSExpr.bool [branch . TmMatch.Split.Bool.Branch.value]]
                  [Doc.braces js_next]
                ]
              ]]
            ]]
            [js_safe_switch js_scrut cases]
          ]]
        ]
      ]]
    ]]]]

    [let root_scrut_jsexpr [compile_tm g [tree . TmMatch.Tree.root_scrut_tm]]]
    [let root_scrut_var [g . JSCodegen.fresh_var]]
    [let switch_jscode [visit_match_node
      [g . JSCodegen.extend_var_env [Env.one root_scrut_var]]
      [tree . TmMatch.Tree.root_node]
    ]]

    [JSExpr.iife [Doc.vcat [Foldable.mconcat [vec
      [Iter.one [JSExpr.const_assign root_scrut_var root_scrut_jsexpr]]
      [Array.iter lines_before_switch]
      [Iter.one switch_jscode]
    ]]]]
  ]]]

  [def-impl compile_tm [\ g in_tm [do
    // NOTE:
    //
    // - Struct encoding: A flat list of fields in order of declaration. Accessed
    //   by index.
    //
    // - Tuple encoding: An actual JavaScript array of fields.
    //
    // - List encoding: An actual JavaScript array.
    //
    // - Dict encoding: An actual JavaScript object with string keys.
    //
    // - Enum member encoding: A 2-item JS array, [<member ID>, <choice fields>]
    //
    //   e.g.,
    //     [class-decl Maybe [type A *]]
    //     [class-enum Maybe
    //       [member Some a] // Member ID = 0
    //       [member None]   // Member ID = 1
    //     ]
    //
    //     // Some "Hello" -> [1, ["Hello"]]
    //     // None         -> [0, []]
    //
    // - Lambda encoding: An actual JS function.
    [match in_tm
      [[Var i] [do
        [g . JSCodegen.var_env . Env.at i . JSExpr.ident]
      ]]
      [[Def uid] [do
        [let def_info [g . JSCodegen.def_info_map . I32MutMap.get uid]]
        [let def_js_name [def_info . DefInfo.js_name]]
        [if [def_info . DefInfo.is_lazily_initialized]
          [JSExpr.ident def_js_name . JSExpr.call [Iter.nil]]
          [JSExpr.ident def_js_name]
        ]
      ]]
      [[App [; is_tail f x]] [do
        [let f [compile_tm g f]]
        [let x [compile_tm g x]]
        [let call [if is_tail "$tailcall" "$apply1"]]
        [JSExpr.call [JSExpr.ident call] [Iter.two f x]]
      ]]
      [[If [; cond on_true on_false]] [do
        [let cond [compile_tm g cond]]
        [let on_true [compile_tm g on_true]]
        [let on_false [compile_tm g on_false]]
        [Doc.parens [JSExpr.conditional_ternary cond on_true on_false]]
      ]]
      [[Error] [do
        [JSExpr.iife [JSExpr.inline "throw new TmError()"]]
      ]]
      [[Lam body] [do
        [let param [g . JSCodegen.fresh_var]]
        [let g* [g . JSCodegen.extend_var_env [Env.one param]]]
        [let body [compile_tm g* body]]
        [JSExpr.arrow [Iter.one param] body]
      ]]
      [[Dict entries] [do
        [JSExpr.object [entries
          . Array.map [\ entry [do
            [let= [; key value] entry]
            [; key [compile_tm g value]]
          ]]
          . Array.iter
        ]]
      ]]
      [[Lit lit] [match lit
        [[I32 i] [JSExpr.i32 i]]
        [[Bool b] [JSExpr.bool b]]
        [[String str] [JSExpr.string str]]
        [[Char c] [JSExpr.string [c . String.from_char]]]
      ]]
      [[Vec elems] [do
        [let elems [elems . Array.map [compile_tm g]]]
        [JSExpr.array [elems . Array.iter]]
      ]]
      [[Tuple elems] [do
        [let elems [elems . Array.map [compile_tm g]]]
        [JSExpr.array [elems . Array.iter]]
      ]]
      [[Class [; _class_uid member_id value]] [do
        [let value [compile_tm g value]]
        [JSExpr.array [Iter.two [JSExpr.i32 member_id] value]]
      ]]
      [[GetTupleIndex [; obj index]] [do
        [let obj [compile_tm g obj]]
        [JSExpr.index obj [JSExpr.i32 index]]
      ]]
      [[SetTupleIndex [; obj index value]] [do
        [let obj [compile_tm g obj]]
        [let value [compile_tm g value]]
        [JSExpr.call_method obj "with" [Iter.two [JSExpr.i32 index] value]]
      ]]
      [[GetDictKey [; dict key]] [do
        [let dict [compile_tm g dict]]
        [JSExpr.index dict [JSExpr.string key]]
      ]]
      [[Let] [do
        [compile_tm.let g in_tm]
      ]]
      [[Match tree] [do
        [compile_tm.match g tree]
      ]]
    ]
  ]]]

  // Returns in order of dependencies to co-dependencies.
  [def-decl toposort_deps [-> Database [Array UID]]]
  [def-impl toposort_deps [\ db [do
    [let output_array [Array.create ,]]

    [let unvisited [db . Database.iter_def_uids . I32MutSet.from_iter]]

    [let* [visit_def [\ [this UID] [do
      [if-let [false] [unvisited . I32MutSet.has this] ,]

      [unvisited . I32MutSet.remove this]
      [output_array . Array.append this]

      [let def [db . Database.get_def this]]
      [let deps_iter [match [def . XDef.tm . get]
        [[None] [Iter.empty]]
        [[Some tm] [Tm.scan_defs tm . I32MutSet.iter]]
      ]]

      [deps_iter . Iter.foreach [\ dep [do
        [visit_def dep]
        continue
      ]]]
    ]]]]

    [loop [\ _ [do
      [if-let [true] [unvisited . I32MutSet.is_empty] break]
      [visit_def [unvisited . I32MutSet.choose]]
      continue
    ]]]
    
    output_array
  ]]]

  [class-decl codegen_js.Params]
  [class-struct codegen_js.Params
    [field db Database]
    [field import_module String]
  ]

  [def-decl pub codegen_js [-> codegen_js.Params JSExpr]]
  [def-impl codegen_js [\ params [do
    [let db [params . codegen_js.Params.db]]
    [let g [JSCodegen.create db]]

    [let def_uids [db . toposort_deps]]

    // Populate 'def_info_map'
    [def_uids . Array.foreach [\ uid [do
      [let def [db . Database.get_def uid]]

      [let foreign_export [var [None]]]
      [let foreign_import [var [None]]]

      [def . XDef.annotations . Array.foreach [\ ann [do
        [ann . String.strip_prefix? "foreign-export:" . Maybe.when_some [\ export_name
          [set foreign_export [Some export_name]]
        ]]
        [ann . String.strip_prefix? "foreign-import:" . Maybe.when_some [\ import_name
          [set foreign_import [Some import_name]]
        ]]
        continue
      ]]]

      [let should_be_lazily_initialized [var false]]
      [def . XDef.tm . get . Maybe.when_some [\ def_tm [do
        [when [def_tm . Tm.count_lam . == 0] [\ _ [do
          [set should_be_lazily_initialized true]
        ]]]
      ]]]

      [let info [DefInfo.new* [dict
        [uid uid]
        [js_name [g
          . JSCodegen.db
          . Database.get_entry uid
          . XEntry.name
          . String.map [\ ch [if [Char.is_alpha_num ch] ch '_']]
          . String.<> "_UID"
          . String.<> [I32.to_str uid]
        ]]
        [foreign_export [get foreign_export]]
        [foreign_import [get foreign_import]]
        [is_lazily_initialized [get should_be_lazily_initialized]]
      ]]]
      [g . JSCodegen.def_info_map . I32MutMap.set uid info]
      continue
    ]]]

    // Generate JS code
    [let js_imports [Array.create [type JSExpr] ,]]
    [let js_export_lines [Array.create [type JSExpr] ,]]
    [let js_def_lines [Array.create [type JSExpr] ,]]

    [def_uids . Array.foreach [\ uid [do
      [let def [db . Database.get_def uid]]
      [let info [g . JSCodegen.def_info_map . I32MutMap.get uid]]

      [def . XDef.tm . get . Maybe.when_some [\ tm [do
        [let js_name [g . JSCodegen.get_def_js_name uid]]
        [let js_expr [compile_tm g tm]]
        [let js_def [Foldable.mconcat [vec
          [JSExpr.inline "var "]
          [JSExpr.ident js_name]
          [JSExpr.inline " = "]
          [if [info . DefInfo.is_lazily_initialized]
            [JSExpr.ident "$create_constant_def"
              . JSExpr.call [Iter.one [JSExpr.arrow [Iter.nil] js_expr]]
            ]
            js_expr
          ]
          [JSExpr.inline ";"]
        ]]]
        [js_def_lines . Array.append js_def]
      ]]]

      [info . DefInfo.foreign_import . Maybe.when_some [\ import_name [do
        [let import [Foldable.mconcat [vec
          [JSExpr.ident import_name]
          [JSExpr.inline " as "]
          [JSExpr.ident [info . DefInfo.js_name]]
        ]]]
        [js_imports . Array.append import]
      ]]]

      [info . DefInfo.foreign_export . Maybe.when_some [\ export_name [do
        [let export [Foldable.mconcat [vec
          [JSExpr.inline "export const "]
          [JSExpr.ident export_name]
          [JSExpr.inline " = "]
          [JSExpr.ident [info . DefInfo.js_name]]
        ]]]
        [js_export_lines . Array.append export]
      ]]]

      continue
    ]]]

    [Doc.vcat [Foldable.mconcat [vec
      [Iter.one [Foldable.mconcat [vec
        [JSExpr.inline "import "]
        [js_imports . Array.iter . Doc.setted]
        [JSExpr.inline " from "]
        [JSExpr.string [params . codegen_js.Params.import_module]]
      ]]]
      [Iter.one [JSExpr.inline prelude_jscode]]
      [Array.iter js_def_lines]
      [Array.iter js_export_lines]
    ]]]
  ]]]
]
