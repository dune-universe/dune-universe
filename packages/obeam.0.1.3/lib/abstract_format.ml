(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Sf = Simple_term_format
module Z = Aux.Z

(* line number *)
type line_t = int
[@@deriving sexp_of]

(* http://erlang.org/doc/apps/erts/absform.html *)
type t =
  | AbstractCode of form_t

and form_t =
  | ModDecl of form_t list

  | AttrExport of {line: line_t;  function_arity_list: (string * int) list}
  | AttrExportType of {line: line_t; type_arity_list: (string * int) list}
  | AttrImport of {line: line_t; module_name: string; function_arity_list: (string * int) list}
  | AttrMod of {line: line_t; module_name: string}
  | AttrFile of {line: line_t; file: string; file_line: line_t}
  | DeclFun of {line: line_t; function_name: string; arity: int; clauses: clause_t list}
  | SpecFun of {line: line_t; module_name: string option; function_name: string; arity: int; specs: type_t list}
  | DeclRecord of {line: line_t; fields: (line_t * string * expr_t option * type_t option) list}
  | DeclType of {line: line_t; name: string; tvars: (line_t * string) list; ty: type_t}
  | DeclOpaqueType of {line: line_t; name: string; tvars: (line_t * string) list; ty: type_t}
  | AttrWild of {line: line_t; attribute: string; term: Sf.t}

  | FormEof

and literal_t =
  | LitAtom of {line: line_t; atom: string}
  | LitChar of {line: line_t; uchar: Uchar.t}
  | LitInteger of {line: line_t; integer: int}
  | LitBigInt of {line: line_t; bigint: Z.t}
  | LitString of {line: line_t; str: string}

and pattern_t =
  | PatCons of {line: line_t; head: pattern_t; tail: pattern_t}
  | PatNil of {line: line_t}
  | PatMap of {line: line_t;  assocs: pattern_assoc_t list}
  | PatTuple of {line: line_t; pats: pattern_t list}
  | PatUniversal of {line: line_t}
  | PatVar of {line: line_t; id: string}
  | PatLit of {lit: literal_t}
and pattern_assoc_t =
  | PatAssocExact of {line: line_t; key: pattern_t; value: pattern_t}

and expr_t =
  | ExprBody of {exprs: expr_t list}
  | ExprCase of {line: line_t; expr: expr_t; clauses: clause_t list}
  | ExprCons of {line: line_t; head: expr_t; tail: expr_t}
  | ExprNil of {line: line_t}
  | ExprListComprehension of {line: line_t; expr: expr_t; qualifiers: qualifier_t list}
  | ExprLocalFunRef of {line: line_t; function_name: string; arity: int}
  | ExprRemoteFunRef of {line: line_t; module_name: atom_or_var_t; function_name: atom_or_var_t; arity: integer_or_var_t}
  | ExprFun of {line: line_t; name: string option; clauses: clause_t list}
  | ExprLocalCall of {line: line_t; function_expr: expr_t; args: expr_t list}
  | ExprRemoteCall of {line: line_t;  line_remote: line_t; module_expr: expr_t; function_expr: expr_t; args: expr_t list}
  | ExprMapCreation of {line: line_t; assocs: expr_assoc_t list}
  | ExprMapUpdate of {line: line_t; map: expr_t; assocs: expr_assoc_t list}
  | ExprMatch of {line: line_t; pattern: pattern_t; body: expr_t}
  | ExprBinOp of {line: line_t; op: string; lhs: expr_t; rhs: expr_t}
  | ExprTuple of {line: line_t; elements: expr_t list}
  | ExprVar of {line: line_t; id: string}
  | ExprLit of {lit: literal_t}
and expr_assoc_t =
  | ExprAssoc of {line: line_t; key: expr_t; value: expr_t}
  | ExprAssocExact of {line: line_t; key: expr_t; value: expr_t}
and qualifier_t =
  | QualifierGenerator of {line: line_t; pattern: pattern_t; expr: expr_t}
  | QualifierFilter of {filter: expr_t}
and atom_or_var_t =
  | AtomVarAtom of {line: line_t; atom: string}
  | AtomVarVar of {line: line_t; id: string}
and integer_or_var_t =
  | IntegerVarInteger of {line: line_t; integer: int}
  | IntegerVarVar of {line: line_t; id: string}

and clause_t =
  | ClsCase of {line: line_t; pattern: pattern_t; guard_sequence: guard_sequence_t option; body: expr_t}
  | ClsFun of {line: line_t; patterns: pattern_t list; guard_sequence: guard_sequence_t option; body: expr_t}

and guard_sequence_t =
  | GuardSeq of {guards: guard_t list}
and guard_t =
  | Guard of {guard_tests: guard_test_t list}
and guard_test_t =
  | GuardTestCall of {line: line_t; function_name: literal_t; args: guard_test_t list}
  | GuardTestMapCreation of {line: line_t; assocs: guard_test_assoc_t list}
  | GuardTestMapUpdate of {line: line_t; map: guard_test_t; assocs: guard_test_assoc_t list}
  | GuardTestBinOp of {line: line_t; op: string; lhs: guard_test_t; rhs: guard_test_t}
  | GuardTestTuple of {line: line_t; elements: guard_test_t list}
  | GuardTestVar of {line: line_t; id: string}
  | GuardTestLit of {lit: literal_t}
and guard_test_assoc_t =
  | GuardTestAssoc of {line: line_t; key: guard_test_t; value: guard_test_t}
  | GuardTestAssocExact of {line: line_t; key: guard_test_t; value: guard_test_t}

and type_t =
  | TyAnn of {line: line_t; annotation: type_t; tyvar: type_t}
  | TyBitstring of {line: line_t; m: type_t; n: type_t}
  | TyPredef of {line: line_t; name: string; args: type_t list}
  | TyBinOp of {line: line_t; op: string; lhs: type_t; rhs: type_t}
  | TyUnaryOp of {line: line_t; op: string; operand: type_t}
  | TyRange of {line: line_t; low: type_t; high: type_t}
  | TyAnyMap of {line: line_t}
  | TyMap of {line: line_t; assocs: type_assoc_t list}
  | TyVar of {line: line_t; id: string}
  | TyFunAny of {line: line_t}
  | TyFunAnyArity of {line: line_t; line_any: line_t; ret: type_t}
  | TyContFun of {line: line_t; function_type: type_t; constraints: type_func_cont_t}
  | TyFun of {line: line_t; line_params: line_t; params: type_t list; ret: type_t}
  | TyAnyTuple of {line: line_t}
  | TyTuple of {line: line_t; elements: type_t list}
  | TyUnion of {line: line_t; elements: type_t list}
  | TyUser of {line: line_t; name: string; args: type_t list}
  | TyLit of {lit: literal_t}
and type_assoc_t =
  | TyAssoc of {line: line_t; key: type_t; value: type_t}
  | TyAssocExact of {line: line_t; key: type_t; value: type_t}

and type_func_cont_t =
  | TyCont of {constraints: type_func_cont_t list}
  | TyContRel of {line: line_t; constraint_kind: type_func_cont_t; lhs: type_t; rhs: type_t}
  | TyContIsSubType of {line: line_t}
[@@deriving sexp_of]

type err_t = Sf.t Err.t
[@@deriving sexp_of]

let track ~loc result =
  Result.map_error ~f:(Err.record_backtrace ~loc:loc) result

(*
 * Entry
 *)
let rec of_sf sf : (t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  | Sf.Tuple (2, [Sf.Atom "raw_abstract_v1"; sf_forms]) ->
     let%bind forms =
       sf_forms |> form_of_sf |> track ~loc:[%here]
     in
     AbstractCode forms |> return

  (* is it suitable here? *)
  | Sf.Tuple (3, [Sf.Atom "debug_info_v1";
                  Sf.Atom "erl_abstract_code";
                  Sf.Tuple (2, [sf_forms; _options])]) ->
     let%bind forms =
       sf_forms |> form_of_sf |> track ~loc:[%here]
     in
     AbstractCode forms |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("root", sf)) |> Result.fail

(*
 * 8.1  Module Declarations and Forms
 *)
and form_of_sf sf : (form_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* module declaration *)
  | Sf.List sf_forms ->
     let%bind forms = sf_forms |> List.map ~f:form_of_sf |> Result.all |> track ~loc:[%here] in
     ModDecl forms |> return

  (* attribute -export *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "export";
                  Sf.List sf_function_arity_list
             ]) ->
     let%bind function_arity_list =
       sf_function_arity_list |> List.map ~f:name_and_arity_of_sf |> Result.all |> track ~loc:[%here]
     in
     AttrExport {line; function_arity_list} |> return

  (* attribute -export_type *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "export_type";
                  Sf.List sf_type_arity_list
             ]) ->
     let%bind type_arity_list =
       sf_type_arity_list |> List.map ~f:name_and_arity_of_sf |> Result.all |> track ~loc:[%here]
     in
     AttrExportType {line; type_arity_list} |> return

  (* attribute -import *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "import";
                  Sf.Tuple (2, [Sf.Atom module_name;
                                Sf.List sf_function_arity_list])
             ]) ->
     let%bind function_arity_list =
       sf_function_arity_list |> List.map ~f:name_and_arity_of_sf |> Result.all |> track ~loc:[%here]
     in
     AttrImport {line; module_name; function_arity_list} |> return

  (* attribute -module *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "module";
                  Sf.Atom module_name
             ]) ->
     AttrMod {line; module_name} |> return

  (* attribute -file *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "file";
                  Sf.Tuple (2, [Sf.String file; Sf.Integer file_line])
             ]) ->
     AttrFile {line; file_line; file} |> return

  (* function declaration *)
  | Sf.Tuple (5, [Sf.Atom "function";
                  Sf.Integer line;
                  Sf.Atom function_name;
                  Sf.Integer arity;
                  Sf.List sf_clauses
             ]) ->
     let%bind clauses =
       sf_clauses |> List.map ~f:(cls_of_sf ~in_function:true) |> Result.all |> track ~loc:[%here]
     in
     DeclFun {line; function_name; arity; clauses} |> return

  (* function specification *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "spec";
                  Sf.Tuple (2, [Sf.Tuple (2, [Sf.Atom function_name; Sf.Integer arity]);
                                Sf.List sf_specs])
             ]) ->
     let%bind specs = sf_specs |> List.map ~f:fun_type_of_sf |> Result.all |> track ~loc:[%here] in
     let module_name = None in
     SpecFun {line; module_name; function_name; arity; specs} |> return

  (* function specification(Mod) *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "spec";
                  Sf.Tuple (2, [Sf.Tuple (3, [Sf.Atom module_name;
                                              Sf.Atom function_name;
                                              Sf.Integer arity]);
                                Sf.List sf_specs])
             ]) ->
     let%bind specs = sf_specs |> List.map ~f:fun_type_of_sf |> Result.all |> track ~loc:[%here] in
     let module_name = Some module_name in
     SpecFun {line; module_name; function_name; arity; specs} |> return

  (* record declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "record";
                  Sf.Tuple (2, [Sf.Atom name; Sf.List sf_fields])
             ]) ->
     let%bind fields =
       sf_fields |> List.map ~f:record_field_of_sf |> Result.all |> track ~loc:[%here]
     in
     DeclRecord {line; fields} |> return

  (* type declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "type";
                  Sf.Tuple (3, [Sf.Atom name;
                                sf_ty;
                                Sf.List sf_tvars
                           ]);
             ]) ->
     let%bind ty = sf_ty |> type_of_sf |> track ~loc:[%here] in
     let%bind tvars = sf_tvars |> List.map ~f:tvar_of_sf |> Result.all |> track ~loc:[%here] in
     DeclType {line; name; tvars; ty} |> return

  (* opaque type declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "opaque";
                  Sf.Tuple (3, [Sf.Atom name;
                                sf_ty;
                                Sf.List sf_tvars
                           ]);
             ]) ->
     let%bind ty = sf_ty |> type_of_sf |> track ~loc:[%here] in
     let%bind tvars = sf_tvars |> List.map ~f:tvar_of_sf |> Result.all |> track ~loc:[%here] in
     DeclOpaqueType {line; name; tvars; ty} |> return

  (* wild attribute *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom attribute;
                  term]) ->
     AttrWild {line; attribute; term} |> return

  (* eof *)
  | Sf.Tuple (2, [Sf.Atom "eof"; Sf.Integer line]) ->
     FormEof |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("form", sf)) |> Result.fail

and name_and_arity_of_sf sf : ((string * int), err_t) Result.t =
  match sf with
  | Sf.Tuple (2, [Sf.Atom name; Sf.Integer arity]) ->
     Ok (name, arity)

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("name_and_arity", sf)) |> Result.fail

and record_field_of_sf sf : ((int * string * expr_t option * type_t option), err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  | Sf.Tuple (3, [Sf.Atom "record_field";
                  Sf.Integer line;
                  Sf.Tuple (3, [Sf.Atom "atom"; _; Sf.Atom field]);
             ]) ->
     (line, field, None, None) |> return

  | Sf.Tuple (4, [Sf.Atom "record_field";
                  Sf.Integer line;
                  Sf.Tuple (3, [Sf.Atom "atom"; _; Sf.Atom field]);
                  sf_e
             ]) ->
     let%bind e = sf_e |> expr_of_sf |> track ~loc:[%here] in
     (line, field, Some e, None) |> return

  | Sf.Tuple (3, [Sf.Atom "typed_record_field";
                  Sf.Tuple (3, [Sf.Atom "record_field";
                                Sf.Integer line;
                                Sf.Tuple (3, [Sf.Atom "atom"; _; Sf.Atom field])
                           ]);
                  sf_t
             ]) ->
     let%bind t = sf_t |> type_of_sf |> track ~loc:[%here] in
     (line, field, None, Some t) |> return

  | Sf.Tuple (3, [Sf.Atom "typed_record_field";
                  Sf.Tuple (4, [Sf.Atom "record_field";
                                Sf.Integer line;
                                Sf.Tuple (3, [Sf.Atom "atom"; _; Sf.Atom field]);
                                sf_e
                           ]);
                  sf_t
             ]) ->
     let%bind e = sf_e |> expr_of_sf |> track ~loc:[%here] in
     let%bind t = sf_t |> type_of_sf |> track ~loc:[%here] in
     (line, field, Some e, Some t) |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("record_field", sf)) |> Result.fail

and tvar_of_sf sf : ((line_t * string), err_t) Result.t =
  match sf with
  | Sf.Tuple (3, [Sf.Atom "var";
                  Sf.Integer line;
                  Sf.Atom tvar
             ]) ->
     Ok (line, tvar)

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("tvar", sf)) |> Result.fail

(*
 * 8.2  Atomic Literals
 *)
and lit_of_sf sf : (literal_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  | Sf.Tuple (3, [Sf.Atom "atom"; Sf.Integer line; Sf.Atom atom]) ->
     LitAtom {line; atom} |> return

  | Sf.Tuple (3, [Sf.Atom "char"; Sf.Integer line; Sf.Integer c]) ->
     begin match Uchar.of_scalar c with
     | None ->
        Err.create ~loc:[%here] (Err.Invalid_input ("not a valid unicode scalar value", sf)) |> Result.fail
     | Some uchar ->
        LitChar {line; uchar} |> return
     end

  | Sf.Tuple (3, [Sf.Atom "float"; Sf.Integer line; _]) ->
     failwith "TODO"

  | Sf.Tuple (3, [Sf.Atom "integer"; Sf.Integer line; Sf.Integer integer]) ->
     LitInteger {line; integer} |> return

  | Sf.Tuple (3, [Sf.Atom "integer"; Sf.Integer line; Sf.BigInt bigint]) ->
     LitBigInt {line; bigint} |> return

  | Sf.Tuple (3, [Sf.Atom "string"; Sf.Integer line; Sf.String str]) ->
     LitString {line; str} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("lit", sf)) |> Result.fail

(*
 * 8.3  Patterns
 *)
and pat_of_sf sf : (pattern_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* a cons pattern *)
  | Sf.Tuple (4, [Sf.Atom "cons"; Sf.Integer line; sf_head; sf_tail]) ->
     let%bind head = sf_head |> pat_of_sf |> track ~loc:[%here] in
     let%bind tail = sf_tail |> pat_of_sf |> track ~loc:[%here] in
     PatCons {line; head; tail} |> return

  (* a nil pattern *)
  | Sf.Tuple (2, [Sf.Atom "nil"; Sf.Integer line]) ->
     PatNil {line} |> return

  (* a map pattern *)
  | Sf.Tuple (3, [Sf.Atom "map"; Sf.Integer line; Sf.List sf_assocs]) ->
     let%bind assocs = sf_assocs |> List.map ~f:pat_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     PatMap {line; assocs} |> return

  (* a tuple pattern *)
  | Sf.Tuple (3, [Sf.Atom "tuple"; Sf.Integer line; Sf.List sf_pats]) ->
     let%bind pats = sf_pats |> List.map ~f:pat_of_sf |> Result.all |> track ~loc:[%here] in
     PatTuple {line; pats} |> return

  (* a variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom "_"]) ->
     PatUniversal {line} |> return

  (* a variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     PatVar {line; id} |> return

  (* atomic literal *)
  | sf_lit ->
     let%bind lit = sf_lit |> lit_of_sf |> track ~loc:[%here] in
     PatLit {lit} |> return

and pat_assoc_of_sf sf : (pattern_assoc_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; sf_key; sf_value]) ->
     let%bind key = sf_key |> pat_of_sf |> track ~loc:[%here] in
     let%bind value = sf_value |> pat_of_sf |> track ~loc:[%here] in
     PatAssocExact {line; key; value} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("pat_assoc", sf)) |> Result.fail

(*
 * 8.4  Expressions
 *)
and expr_of_sf sf : (expr_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  | Sf.List sf_exprs ->
     let%bind exprs = sf_exprs |> List.map ~f:expr_of_sf |> Result.all |> track ~loc:[%here] in
     ExprBody {exprs} |> return

  (* a case expression *)
  | Sf.Tuple (4, [Sf.Atom "case"; Sf.Integer line; sf_expr; Sf.List sf_clauses]) ->
     let%bind expr = sf_expr |> expr_of_sf |> track ~loc:[%here] in
     let%bind clauses = sf_clauses |> List.map ~f:cls_of_sf |> Result.all |> track ~loc:[%here] in
     ExprCase {line; expr; clauses} |> return

  (* a cons expression *)
  | Sf.Tuple (4, [Sf.Atom "cons"; Sf.Integer line; sf_head; sf_tail]) ->
     let%bind head = sf_head |> expr_of_sf |> track ~loc:[%here] in
     let%bind tail = sf_tail |> expr_of_sf |> track ~loc:[%here] in
     ExprCons {line; head; tail} |> return

  (* a nil expression *)
  | Sf.Tuple (2, [Sf.Atom "nil"; Sf.Integer line]) ->
     ExprNil {line} |> return

  (* a list comprehension *)
  | Sf.Tuple (4, [Sf.Atom "lc";
                  Sf.Integer line;
                  sf_expr;
                  Sf.List sf_qualifiers]) ->
     let%bind expr = sf_expr |> expr_of_sf |> track ~loc:[%here] in
     let%bind qualifiers = sf_qualifiers |> List.map ~f:qualifier_of_sf |> Result.all |> track ~loc:[%here] in
     ExprListComprehension {line; expr; qualifiers} |> return

  (* a local function reference *)
  | Sf.Tuple (3, [Sf.Atom "fun";
                  Sf.Integer line;
                  Sf.Tuple (3, [Sf.Atom "function";
                                Sf.Atom function_name;
                                Sf.Integer arity])]) ->
    ExprLocalFunRef {line; function_name; arity} |> return

  (* a remote function reference *)
  | Sf.Tuple (3, [Sf.Atom "fun";
                  Sf.Integer line;
                  Sf.Tuple (4, [Sf.Atom "function";
                                sf_module_name;
                                sf_function_name;
                                sf_arity])]) ->
    let%bind module_name = sf_module_name |> atom_or_var_of_sf |> track ~loc:[%here] in
    let%bind function_name = sf_function_name |> atom_or_var_of_sf |> track ~loc:[%here] in
    let%bind arity = sf_arity |> integer_or_var_of_sf |> track ~loc:[%here] in
    ExprRemoteFunRef {line; module_name; function_name; arity} |> return

  (* a function expression *)
  | Sf.Tuple (3, [Sf.Atom "fun";
                  Sf.Integer line;
                  Sf.Tuple (2, [Sf.Atom "clauses";
                                Sf.List sf_clauses])]) ->
    let%bind clauses = sf_clauses |> List.map ~f:(cls_of_sf ~in_function:true) |> Result.all |> track ~loc:[%here] in
    ExprFun {line; name = None; clauses} |> return

  (* a named function expression *)
  | Sf.Tuple (4, [Sf.Atom "named_fun";
                  Sf.Integer line;
                  Sf.Atom name;
                  Sf.List sf_clauses]) ->
    let%bind clauses = sf_clauses |> List.map ~f:(cls_of_sf ~in_function:true) |> Result.all |> track ~loc:[%here] in
    ExprFun {line; name = Some name; clauses} |> return

  (* a function call (remote) *)
  | Sf.Tuple (4, [Sf.Atom "call";
                  Sf.Integer line;
                  Sf.Tuple (4, [Sf.Atom "remote"; Sf.Integer line_remote; sf_module_expr; sf_function_expr]);
                  Sf.List sf_args]) ->
     let%bind module_expr = sf_module_expr |> expr_of_sf |> track ~loc:[%here] in
     let%bind function_expr = sf_function_expr |> expr_of_sf |> track ~loc:[%here] in
     let%bind args = sf_args |> List.map ~f:expr_of_sf |> Result.all |> track ~loc:[%here] in
     ExprRemoteCall {line; line_remote; module_expr; function_expr; args} |> return

  (* a function call (local) *)
  | Sf.Tuple (4, [Sf.Atom "call"; Sf.Integer line; sf_function_expr; Sf.List sf_args]) ->
     let%bind function_expr = sf_function_expr |> expr_of_sf |> track ~loc:[%here] in
     let%bind args = sf_args |> List.map ~f:expr_of_sf |> Result.all |> track ~loc:[%here] in
     ExprLocalCall {line; function_expr; args} |> return

  (* a map creation *)
  | Sf.Tuple (3, [Sf.Atom "map";
                  Sf.Integer line;
                  Sf.List sf_assocs]) ->
     let%bind assocs = sf_assocs |> List.map ~f:expr_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     ExprMapCreation {line; assocs} |> return

  (* a map update *)
  | Sf.Tuple (4, [Sf.Atom "map";
                  Sf.Integer line;
                  sf_map;
                  Sf.List sf_assocs]) ->
     let%bind map = sf_map |> expr_of_sf |> track ~loc:[%here] in
     let%bind assocs = sf_assocs |> List.map ~f:expr_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     ExprMapUpdate {line; map; assocs} |> return

  (* match operator expression *)
  | Sf.Tuple (4, [Sf.Atom "match";
                  Sf.Integer line;
                  sf_pattern;
                  sf_body]) ->
     let%bind pattern = sf_pattern |> pat_of_sf |> track ~loc:[%here] in
     let%bind body = sf_body |> expr_of_sf |> track ~loc:[%here] in
     ExprMatch {line; pattern; body} |> return

  (* an operator expression binary *)
  | Sf.Tuple (5, [Sf.Atom "op";
                  Sf.Integer line;
                  Sf.Atom op;
                  sf_lhs;
                  sf_rhs]) ->
     let%bind lhs = sf_lhs |> expr_of_sf |> track ~loc:[%here] in
     let%bind rhs = sf_rhs |> expr_of_sf |> track ~loc:[%here] in
     ExprBinOp {line; op; lhs; rhs} |> return

  (* a tuple skeleton *)
  | Sf.Tuple (3, [Sf.Atom "tuple";
                  Sf.Integer line;
                  Sf.List sf_elements]) ->
     let%bind elements = sf_elements |> List.map ~f:expr_of_sf |> Result.all |> track ~loc:[%here] in
     ExprTuple {line; elements} |> return

  (* a variable *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     ExprVar {line; id} |> return

  (* atomic literal *)
  | sf_lit ->
     let%bind lit = sf_lit |> lit_of_sf |> track ~loc:[%here] in
     ExprLit {lit} |> return

and expr_assoc_of_sf sf : (expr_assoc_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* an association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_assoc"; Sf.Integer line; sf_key; sf_value]) ->
     let%bind key = sf_key |> expr_of_sf |> track ~loc:[%here] in
     let%bind value = sf_value |> expr_of_sf |> track ~loc:[%here] in
     ExprAssoc {line; key; value} |> return

  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; sf_key; sf_value]) ->
     let%bind key = sf_key |> expr_of_sf |> track ~loc:[%here] in
     let%bind value = sf_value |> expr_of_sf |> track ~loc:[%here] in
     ExprAssocExact {line; key; value} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("expr_assoc", sf)) |> Result.fail

and qualifier_of_sf sf : (qualifier_t, err_t) Result.t =
    let open Result.Let_syntax in
    match sf with
    (* generator qualifier *)
    | Sf.Tuple (4, [Sf.Atom "generate"; Sf.Integer line; sf_pattern; sf_expr]) ->
       let%bind pattern = sf_pattern |> pat_of_sf |> track ~loc:[%here] in
       let%bind expr = sf_expr |> expr_of_sf |> track ~loc:[%here] in
       QualifierGenerator {line; pattern; expr} |> return
    (* filter qualifier *)
    | sf_filter ->
       let%bind filter = sf_filter |> expr_of_sf |> track ~loc:[%here] in
       QualifierFilter {filter} |> return

and atom_or_var_of_sf sf : (atom_or_var_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* atom *)
  | (Sf.Tuple (3, [(Sf.Atom "atom");
                   (Sf.Integer line);
                   (Sf.Atom atom)])) ->
    AtomVarAtom {line; atom} |> return
  (* variable *)
  | (Sf.Tuple (3, [(Sf.Atom "var");
                   (Sf.Integer line);
                   (Sf.Atom id)])) ->
    AtomVarVar {line; id} |> return
  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("atom_or_var", sf)) |> Result.fail

and integer_or_var_of_sf sf =
  let open Result.Let_syntax in
  match sf with
  (* integer *)
  | (Sf.Tuple (3, [(Sf.Atom "integer");
                   (Sf.Integer line);
                   (Sf.Integer integer)])) ->
    IntegerVarInteger {line; integer} |> return
  (* variable *)
  | (Sf.Tuple (3, [(Sf.Atom "var");
                   (Sf.Integer line);
                   (Sf.Atom id)])) ->
    IntegerVarVar {line; id} |> return
  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("integer_or_var", sf)) |> Result.fail

(*
 * 8.5  Clauses
 *)
and cls_of_sf ?(in_function=false) sf : (clause_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf, in_function with
  (* case clause P -> B *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List [sf_pattern];
                 Sf.List [];
                 sf_body
             ]), false ->
     let%bind pattern = sf_pattern |> pat_of_sf |> track ~loc:[%here] in
     let%bind body = sf_body |> expr_of_sf |> track ~loc:[%here] in
     ClsCase {line; pattern; guard_sequence = None; body} |> return

  (* case clause P -> B when Gs *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List [sf_pattern];
                 sf_guard_sequence;
                 sf_body
             ]), false ->
     let%bind pattern = sf_pattern |> pat_of_sf |> track ~loc:[%here] in
     let%bind guard_sequence = sf_guard_sequence |> guard_sequence_of_sf |> track ~loc:[%here] in
     let%bind body = sf_body |> expr_of_sf |> track ~loc:[%here] in
     ClsCase {line; pattern; guard_sequence = Some guard_sequence; body} |> return

  (* function clause ( Ps ) -> B *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List sf_patterns;
                 Sf.List [];
                 sf_body
             ]), true ->
     let%bind patterns = sf_patterns |> List.map ~f:pat_of_sf |> Result.all |> track ~loc:[%here] in
     let%bind body = sf_body |> expr_of_sf |> track ~loc:[%here] in
     ClsFun {line; patterns; guard_sequence = None; body} |> return

  (* function clause ( Ps ) when Gs -> B *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List sf_patterns;
                 sf_guard_sequence;
                 sf_body
             ]), true ->
     let%bind patterns = sf_patterns |> List.map ~f:pat_of_sf |> Result.all |> track ~loc:[%here] in
     let%bind guard_sequence = sf_guard_sequence |> guard_sequence_of_sf |> track ~loc:[%here] in
     let%bind body = sf_body |> expr_of_sf |> track ~loc:[%here] in
     ClsFun {line; patterns; guard_sequence = Some guard_sequence; body} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("cls", sf)) |> Result.fail

(*
 * 8.6  Guards
 *)
and guard_sequence_of_sf sf : (guard_sequence_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* empty or non-empty sequence *)
  | Sf.List sf_guards ->
     let%bind guards = sf_guards |> List.map ~f:guard_of_sf |> Result.all |> track ~loc:[%here] in
     GuardSeq {guards} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("guard_sequence", sf)) |> Result.fail

and guard_of_sf sf : (guard_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* non-empty sequence *)
  | Sf.List sf_guard_tests when List.length sf_guard_tests > 0 ->
     let%bind guard_tests = sf_guard_tests |> List.map ~f:guard_test_of_sf |> Result.all |> track ~loc:[%here] in
     Guard {guard_tests} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("guard", sf)) |> Result.fail

and guard_test_of_sf sf : (guard_test_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* function call *)
  | Sf.Tuple (4, [
                 Sf.Atom "call";
                 Sf.Integer line;
                 sf_function_name;
                 Sf.List sf_args
             ]) ->
     let%bind function_name = sf_function_name |> lit_of_sf |> track ~loc:[%here] in
     let%bind args =  sf_args |> List.map ~f:guard_test_of_sf |> Result.all |> track ~loc:[%here] in
     GuardTestCall {line; function_name; args} |> return

  (* a map creation *)
  | Sf.Tuple (3, [Sf.Atom "map"; Sf.Integer line; Sf.List sf_assocs]) ->
     let%bind assocs = sf_assocs |> List.map ~f:guard_test_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     GuardTestMapCreation {line; assocs} |> return

  (* a map update *)
  | Sf.Tuple (4, [Sf.Atom "map"; Sf.Integer line; sf_map; Sf.List sf_assocs]) ->
     let%bind map = sf_map |> guard_test_of_sf |> track ~loc:[%here] in
     let%bind assocs = sf_assocs |> List.map ~f:guard_test_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     GuardTestMapUpdate {line; map; assocs} |> return

  (* a binary operator *)
  | Sf.Tuple (5, [Sf.Atom "op"; Sf.Integer line; Sf.Atom op; sf_lhs; sf_rhs]) ->
     let%bind lhs = sf_lhs |> guard_test_of_sf |> track ~loc:[%here] in
     let%bind rhs = sf_rhs |> guard_test_of_sf |> track ~loc:[%here] in
     GuardTestBinOp {line; op; lhs; rhs} |> return

  (* a tuple skeleton *)
  | Sf.Tuple (3, [Sf.Atom "tuple"; Sf.Integer line; Sf.List sf_elements]) ->
     let%bind elements = sf_elements |> List.map ~f:guard_test_of_sf |> Result.all |> track ~loc:[%here] in
     GuardTestTuple {line; elements} |> return

  (* variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     GuardTestVar {line; id} |> return

  (* atomic literal *)
  | sf_lit ->
     let%bind lit = sf_lit |> lit_of_sf |> track ~loc:[%here] in
     GuardTestLit {lit} |> return

and guard_test_assoc_of_sf sf : (guard_test_assoc_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* an association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_assoc"; Sf.Integer line; sf_key; sf_value]) ->
     let%bind key = sf_key |> guard_test_of_sf |> track ~loc:[%here]in
     let%bind value = sf_value |> guard_test_of_sf |> track ~loc:[%here] in
     GuardTestAssoc {line; key; value} |> return

  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; sf_key; sf_value]) ->
     let%bind key = sf_key |> guard_test_of_sf |> track ~loc:[%here] in
     let%bind value = sf_value |> guard_test_of_sf |> track ~loc:[%here]in
     GuardTestAssocExact {line; key; value} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("guard_test_assoc", sf)) |> Result.fail

(*
 * 8.7  Types
 *)
and type_of_sf sf : (type_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* annotated type *)
  | Sf.Tuple (3, [Sf.Atom "ann_type";
                  Sf.Integer line;
                  Sf.List [sf_annotation; sf_tyvar]]) ->
     let%bind annotation = sf_annotation |> type_of_sf |> track ~loc:[%here] in
     let%bind tyvar = sf_tyvar |> type_of_sf |> track ~loc:[%here] in
     TyAnn {line; annotation; tyvar} |> return

  (* bitstring type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "binary";
                  Sf.List [sf_m; sf_n]]) ->
     let%bind m = sf_m |> type_of_sf |> track ~loc:[%here] in
     let%bind n = sf_n |> type_of_sf |> track ~loc:[%here] in
     TyBitstring {line; m; n} |> return

  (* fun type (any) *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "fun";
                  Sf.List []]) ->
     TyFunAny {line} |> return

  (* fun type (any arity) *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "fun";
                  Sf.List [Sf.Tuple (3, [
                                       Sf.Atom "type";
                                       Sf.Integer line_any;
                                       Sf.Atom "any";
                                    ]);
                           sf_ret]]) ->
     let%bind ret = sf_ret |> type_of_sf |> track ~loc:[%here] in
     TyFunAnyArity {line; line_any; ret} |> return

  (* map type (any) *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "map";
                  Sf.Atom "any"]) ->
     TyAnyMap {line} |> return

  (* map type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "map";
                  Sf.List sf_assocs]) ->
     let%bind assocs =
       sf_assocs |> List.map ~f:type_assoc_of_sf |> Result.all |> track ~loc:[%here]
     in
     TyMap {line; assocs} |> return

  (* operator type for a binary operator *)
  | Sf.Tuple (5, [Sf.Atom "op";
                  Sf.Integer line;
                  Sf.Atom op;
                  sf_lhs; sf_rhs]) ->
     let%bind lhs = sf_lhs |> type_of_sf |> track ~loc:[%here] in
     let%bind rhs = sf_rhs |> type_of_sf |> track ~loc:[%here] in
     TyBinOp {line; lhs; op; rhs} |> return

  (* operator type for a unary operator *)
  | Sf.Tuple (4, [Sf.Atom "op";
                  Sf.Integer line;
                  Sf.Atom op;
                  sf_operand]) ->
     let%bind operand = sf_operand |> type_of_sf |> track ~loc:[%here] in
     TyUnaryOp {line; op; operand} |> return

  (* range type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "range";
                  Sf.List [sf_low; sf_high]]) ->
     let%bind low = sf_low |> type_of_sf |> track ~loc:[%here] in
     let%bind high = sf_high |> type_of_sf |> track ~loc:[%here] in
     TyRange {line; low; high} |> return

  (* tuple type (any) *)
  | Sf.Tuple (4, [Sf.Atom "type"; Sf.Integer line; Sf.Atom "tuple"; Sf.Atom "any"]) ->
     TyAnyTuple {line} |> return

  (* tuple type *)
  | Sf.Tuple (4, [Sf.Atom "type"; Sf.Integer line; Sf.Atom "tuple"; Sf.List sf_elements]) ->
     let%bind elements = sf_elements |> List.map ~f:type_of_sf |> Result.all |> track ~loc:[%here] in
     TyTuple {line; elements} |> return

  (* union type *)
  | Sf.Tuple  (4, [Sf.Atom "type"; Sf.Integer line; Sf.Atom "union"; Sf.List sf_elements]) ->
     let%bind elements = sf_elements |> List.map ~f:type_of_sf |> Result.all |> track ~loc:[%here] in
     TyUnion {line; elements} |> return

  (* predefined (or built-in) type OR fun type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom name;
                  Sf.List sf_args]) ->
     begin match fun_type_of_sf sf with
     | Ok fn -> Ok fn
     | _ ->
        let%bind args =
          sf_args |> List.map ~f:type_of_sf |> Result.all |> track ~loc:[%here]
        in
        TyPredef {line; name; args} |> return
     end

  (* type variable *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     TyVar {line; id} |> return

  (* user defined type *)
  | Sf.Tuple (4, [Sf.Atom "user_type";
                  Sf.Integer line;
                  Sf.Atom name;
                  Sf.List sf_args]) ->
     let%bind args =
       sf_args |> List.map ~f:type_of_sf |> Result.all |> track ~loc:[%here]
     in
     TyUser {line; name; args} |> return

  (* atomic literal *)
  | sf_lit ->
     let%bind lit = sf_lit |> lit_of_sf |> track ~loc:[%here] in
     TyLit {lit} |> return

and fun_type_of_sf sf : (type_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* constrained function type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "bounded_fun";
                  Sf.List [sf_function_type; sf_constraints]
             ]) ->
     let%bind function_type = sf_function_type |> type_of_sf |> track ~loc:[%here] in
     let%bind constraints = sf_constraints |> type_fun_cont_of_sf |> track ~loc:[%here] in
     TyContFun {line; function_type; constraints} |> return

  (* function type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "fun";
                  Sf.List [Sf.Tuple (4, [Sf.Atom "type";
                                         Sf.Integer line_params;
                                         Sf.Atom "product";
                                         Sf.List sf_params]);
                           sf_ret]]) ->
     let%bind params = sf_params |> List.map ~f:type_of_sf |> Result.all |> track ~loc:[%here] in
     let%bind ret = sf_ret |> type_of_sf |> track ~loc:[%here] in
     TyFun {line; line_params; params; ret} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("fun_type", sf)) |> Result.fail

and type_fun_cont_of_sf sf : (type_func_cont_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  | Sf.List sf_constraints ->
     let%bind constraints =
       sf_constraints |> List.map ~f:type_fun_cont_of_sf |> Result.all |> track ~loc:[%here]
     in
     TyCont {constraints} |> return

  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "constraint";
                  Sf.List [sf_constraint_kind; Sf.List [sf_lhs; sf_rhs]]
             ]) ->
     let%bind constraint_kind = sf_constraint_kind |> type_fun_cont_of_sf |> track ~loc:[%here] in
     let%bind lhs = sf_lhs |> type_of_sf |> track ~loc:[%here] in
     let%bind rhs = sf_rhs |> type_of_sf |> track ~loc:[%here] in
     TyContRel {line; constraint_kind; lhs; rhs} |> return

  | Sf.Tuple (3, [Sf.Atom "atom"; Sf.Integer line; Sf.Atom "is_subtype"]) ->
     TyContIsSubType {line} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("type_fun_cont", sf)) |> Result.fail

and type_assoc_of_sf sf : (type_assoc_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* an association *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "map_field_assoc";
                  Sf.List [sf_key; sf_value]]) ->
     let%bind key = sf_key |> type_of_sf |> track ~loc:[%here] in
     let%bind value = sf_value |> type_of_sf |> track ~loc:[%here] in
     TyAssoc {line; key; value} |> return

  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "map_field_exact";
                  Sf.List [sf_key; sf_value]]) ->
     let%bind key = sf_key |> type_of_sf |> track ~loc:[%here] in
     let%bind value = sf_value |> type_of_sf |> track ~loc:[%here] in
     TyAssocExact {line; key; value} |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("type_assoc", sf)) |> Result.fail

(**)
let of_etf etf : (t, err_t) Result.t =
  etf |> Sf.of_etf |> of_sf
