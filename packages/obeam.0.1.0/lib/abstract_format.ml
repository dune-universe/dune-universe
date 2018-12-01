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

  | AttrExport of line_t * (string * int) list
  | AttrExportType of line_t * (string * int) list
  | AttrImport of line_t * (string * int) list
  | AttrMod of line_t * string
  | AttrFile of line_t * string * line_t
  | DeclFun of line_t * string * int * clause_t list
  | SpecFun of line_t * string option * string * int * type_t list
  | DeclRecord of line_t * (line_t * string * expr_t option * type_t option) list
  | DeclType of line_t * string * (line_t * string) list * type_t
  | DeclOpaqueType of line_t * string * (line_t * string) list * type_t
  | AttrWild of line_t * string * Sf.t

  | FormEof

and literal_t =
  | LitAtom of line_t * string
  | LitInteger of line_t * int
  | LitBigInt of line_t * Z.t
  | LitString of line_t * string

and pattern_t =
  | PatMap of line_t * pattern_assoc_t list
  | PatUniversal of line_t
  | PatVar of line_t * string
  | PatLit of literal_t
and pattern_assoc_t =
  | PatAssocExact of line_t * pattern_t * pattern_t

and expr_t =
  | ExprBody of expr_t list
  | ExprCase of line_t * expr_t * clause_t list
  | ExprLocalCall of line_t * expr_t * expr_t list
  | ExprRemoteCall of line_t * line_t * expr_t * expr_t * expr_t list
  | ExprMapCreation of line_t * expr_assoc_t list
  | ExprMapUpdate of line_t * expr_t * expr_assoc_t list
  | ExprBinOp of line_t * string * expr_t * expr_t
  | ExprVar of line_t * string
  | ExprLit of literal_t
and expr_assoc_t =
  | ExprAssoc of line_t * expr_t * expr_t
  | ExprAssocExact of line_t * expr_t * expr_t

and clause_t =
  | ClsCase of line_t * pattern_t * guard_sequence_t option * expr_t
  | ClsFun of line_t * pattern_t list * guard_sequence_t option * expr_t

and guard_sequence_t =
  | GuardSeq of guard_t list
and guard_t =
  | Guard of guard_test_t list
and guard_test_t =
  | GuardTestCall of line_t * literal_t * guard_test_t list
  | GuardTestMapCreation of line_t * guard_test_assoc_t list
  | GuardTestMapUpdate of line_t * guard_test_t * guard_test_assoc_t list
  | GuardTestBinOp of line_t * string * guard_test_t * guard_test_t
  | GuardTestVar of line_t * string
  | GuardTestLit of literal_t
and guard_test_assoc_t =
  | GuardTestAssoc of line_t * guard_test_t * guard_test_t
  | GuardTestAssocExact of line_t * guard_test_t * guard_test_t

and type_t =
  | TyAnn of line_t * type_t * type_t
  | TyPredef of line_t * string * type_t list
  | TyProduct of line_t * type_t list
  | TyVar of line_t * string
  | TyContFun of line_t * type_t * type_func_cont_t
  | TyFun of line_t * type_t * type_t

and type_func_cont_t =
  | TyCont of type_func_cont_t list
  | TyContRel of line_t * type_func_cont_t * type_t * type_t
  | TyContIsSubType of line_t
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
                  Sf.List sf_name_and_arity_list
             ]) ->
     let%bind name_and_arity_list =
       sf_name_and_arity_list |> List.map ~f:name_and_arity_of_sf |> Result.all |> track ~loc:[%here]
     in
     AttrExport (line, name_and_arity_list) |> return

  (* attribute -export_type *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "export_type";
                  Sf.List sf_name_and_arity_list
             ]) ->
     let%bind name_and_arity_list =
       sf_name_and_arity_list |> List.map ~f:name_and_arity_of_sf |> Result.all |> track ~loc:[%here]
     in
     AttrExportType (line, name_and_arity_list) |> return

  (* attribute -import *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "import";
                  Sf.List sf_name_and_arity_list
             ]) ->
     let%bind name_and_arity_list =
       sf_name_and_arity_list |> List.map ~f:name_and_arity_of_sf |> Result.all |> track ~loc:[%here]
     in
     AttrImport (line, name_and_arity_list) |> return

  (* attribute -module *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "module";
                  Sf.Atom module_name
             ]) ->
     AttrMod (line, module_name) |> return

  (* attribute -file *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "file";
                  Sf.Tuple (2, [Sf.String file; Sf.Integer file_line])
             ]) ->
     AttrFile (line, file, file_line) |> return

  (* function declaration *)
  | Sf.Tuple (5, [Sf.Atom "function";
                  Sf.Integer line;
                  Sf.Atom name;
                  Sf.Integer arity;
                  Sf.List sf_f_clauses
             ]) ->
     let%bind f_clauses =
       sf_f_clauses |> List.map ~f:(cls_of_sf ~in_function:true) |> Result.all |> track ~loc:[%here]
     in
     DeclFun (line, name, arity, f_clauses) |> return

  (* function specification *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "spec";
                  Sf.Tuple (2, [Sf.Tuple (2, [Sf.Atom name; Sf.Integer arity]);
                                Sf.List sf_specs])
             ]) ->
     let%bind specs = sf_specs |> List.map ~f:fun_type_of_sf |> Result.all |> track ~loc:[%here] in
     SpecFun (line, None, name, arity, specs) |> return

  (* function specification(Mod) *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "spec";
                  Sf.Tuple (2, [Sf.Tuple (3, [Sf.Atom m; Sf.Atom name; Sf.Integer arity]);
                                Sf.List sf_specs])
             ]) ->
     let%bind specs = sf_specs |> List.map ~f:fun_type_of_sf |> Result.all |> track ~loc:[%here] in
     SpecFun (line, Some m, name, arity, specs) |> return

  (* record declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "record";
                  Sf.Tuple (2, [Sf.Atom name; Sf.List sf_record_fields])
             ]) ->
     let%bind record_fields =
       sf_record_fields |> List.map ~f:record_field_of_sf |> Result.all |> track ~loc:[%here]
     in
     DeclRecord (line, record_fields) |> return

  (* type declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "type";
                  Sf.Tuple (3, [Sf.Atom name;
                                sf_t;
                                Sf.List sf_tvars
                           ]);
             ]) ->
     let%bind t = sf_t |> type_of_sf |> track ~loc:[%here] in
     let%bind tvars = sf_tvars |> List.map ~f:tvar_of_sf |> Result.all |> track ~loc:[%here] in
     DeclType (line, name, tvars, t) |> return

  (* opaque type declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "opaque";
                  Sf.Tuple (3, [Sf.Atom name;
                                sf_t;
                                Sf.List sf_tvars
                           ]);
             ]) ->
     let%bind t = sf_t |> type_of_sf |> track ~loc:[%here] in
     let%bind tvars = sf_tvars |> List.map ~f:tvar_of_sf |> Result.all |> track ~loc:[%here] in
     DeclOpaqueType (line, name, tvars, t) |> return

  (* wild attribute *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom attr;
                  term]) ->
     AttrWild (line, attr, term) |> return

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
  | Sf.Tuple (3, [Sf.Atom "atom"; Sf.Integer line; Sf.Atom v]) ->
     LitAtom (line, v) |> return

  | Sf.Tuple (3, [Sf.Atom "char"; Sf.Integer line; _]) ->
     failwith "TODO"

  | Sf.Tuple (3, [Sf.Atom "float"; Sf.Integer line; _]) ->
     failwith "TODO"

  | Sf.Tuple (3, [Sf.Atom "integer"; Sf.Integer line; Sf.Integer v]) ->
     LitInteger (line, v) |> return

  | Sf.Tuple (3, [Sf.Atom "integer"; Sf.Integer line; Sf.BigInt v]) ->
     LitBigInt (line, v) |> return

  | Sf.Tuple (3, [Sf.Atom "string"; Sf.Integer line; Sf.String v]) ->
     LitString (line, v) |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("lit", sf)) |> Result.fail

(*
 * 8.3  Patterns
 *)
and pat_of_sf sf : (pattern_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* a map pattern *)
  | Sf.Tuple (3, [Sf.Atom "map"; Sf.Integer line; Sf.List sf_assocs]) ->
     let%bind assocs = sf_assocs |> List.map ~f:pat_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     PatMap (line, assocs) |> return

  (* a variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom "_"]) ->
     PatUniversal line |> return

  (* a variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     PatVar (line, id) |> return

  (* atomic literal *)
  | sf_v ->
     let%bind v = sf_v |> lit_of_sf |> track ~loc:[%here] in
     PatLit v |> return

and pat_assoc_of_sf sf : (pattern_assoc_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; sf_k; sf_v]) ->
     let%bind k = sf_k |> pat_of_sf |> track ~loc:[%here] in
     let%bind v = sf_v |> pat_of_sf |> track ~loc:[%here] in
     PatAssocExact (line, k, v) |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("pat_assoc", sf)) |> Result.fail

(*
 * 8.4  Expressions
 *)
and expr_of_sf sf : (expr_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  | Sf.List sf_es ->
     let%bind es = sf_es |> List.map ~f:expr_of_sf |> Result.all |> track ~loc:[%here] in
     ExprBody es |> return

  (* a case expression *)
  | Sf.Tuple (4, [Sf.Atom "case"; Sf.Integer line; sf_e; Sf.List sf_clauses]) ->
     let%bind e = sf_e |> expr_of_sf |> track ~loc:[%here] in
     let%bind clauses = sf_clauses |> List.map ~f:cls_of_sf |> Result.all |> track ~loc:[%here] in
     ExprCase (line, e, clauses) |> return

  (* a function call (remote) *)
  | Sf.Tuple (4, [Sf.Atom "call";
                  Sf.Integer line_c;
                  Sf.Tuple (4, [Sf.Atom "remote"; Sf.Integer line_r; sf_m; sf_f]);
                  Sf.List sf_args]) ->
     let%bind m = sf_m |> expr_of_sf |> track ~loc:[%here] in
     let%bind f = sf_f |> expr_of_sf |> track ~loc:[%here] in
     let%bind args = sf_args |> List.map ~f:expr_of_sf |> Result.all |> track ~loc:[%here] in
     ExprRemoteCall (line_c, line_r, m, f, args) |> return

  (* a function call (local) *)
  | Sf.Tuple (4, [Sf.Atom "call"; Sf.Integer line; sf_e; Sf.List sf_es]) ->
     let%bind e = sf_e |> expr_of_sf |> track ~loc:[%here] in
     let%bind es = sf_es |> List.map ~f:expr_of_sf |> Result.all |> track ~loc:[%here] in
     ExprLocalCall (line, e, es) |> return

  (* a map creation *)
  | Sf.Tuple (3, [Sf.Atom "map";
                  Sf.Integer line;
                  Sf.List sf_assocs]) ->
     let%bind assocs = sf_assocs |> List.map ~f:expr_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     ExprMapCreation (line, assocs) |> return

  (* a map update *)
  | Sf.Tuple (4, [Sf.Atom "map";
                  Sf.Integer line;
                  sf_m;
                  Sf.List sf_assocs]) ->
     let%bind m = sf_m |> expr_of_sf |> track ~loc:[%here] in
     let%bind assocs = sf_assocs |> List.map ~f:expr_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     ExprMapUpdate (line, m, assocs) |> return

  (* an operator expression binary *)
  | Sf.Tuple (5, [Sf.Atom "op";
                  Sf.Integer line;
                  Sf.Atom op;
                  sf_p1;
                  sf_p2]) ->
     let%bind p1 = sf_p1 |> expr_of_sf |> track ~loc:[%here] in
     let%bind p2 = sf_p2 |> expr_of_sf |> track ~loc:[%here] in
     ExprBinOp (line, op, p1, p2) |> return

  (* a variable *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     ExprVar (line, id) |> return

  (* atomic literal *)
  | sf_v ->
     let%bind v = sf_v |> lit_of_sf |> track ~loc:[%here] in
     ExprLit v |> return

and expr_assoc_of_sf sf : (expr_assoc_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* an association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_assoc"; Sf.Integer line; sf_k; sf_v]) ->
     let%bind k = sf_k |> expr_of_sf |> track ~loc:[%here] in
     let%bind v = sf_v |> expr_of_sf |> track ~loc:[%here] in
     ExprAssoc (line, k, v) |> return

  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; sf_k; sf_v]) ->
     let%bind k = sf_k |> expr_of_sf |> track ~loc:[%here] in
     let%bind v = sf_v |> expr_of_sf |> track ~loc:[%here] in
     ExprAssocExact (line, k, v) |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("expr_assoc", sf)) |> Result.fail

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
     ClsCase (line, pattern, None, body) |> return

  (* case clause P -> B when Gs *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List [sf_pattern];
                 sf_guards;
                 sf_body
             ]), false ->
     let%bind pattern = sf_pattern |> pat_of_sf |> track ~loc:[%here] in
     let%bind guards = sf_guards |> guard_sequence_of_sf |> track ~loc:[%here] in
     let%bind body = sf_body |> expr_of_sf |> track ~loc:[%here] in
     ClsCase (line, pattern, Some guards, body) |> return

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
     ClsFun (line, patterns, None, body) |> return

  (* function clause ( Ps ) when Gs -> B *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List sf_patterns;
                 sf_guards;
                 sf_body
             ]), true ->
     let%bind patterns = sf_patterns |> List.map ~f:pat_of_sf |> Result.all |> track ~loc:[%here] in
     let%bind guards = sf_guards |> guard_sequence_of_sf |> track ~loc:[%here] in
     let%bind body = sf_body |> expr_of_sf |> track ~loc:[%here] in
     ClsFun (line, patterns, Some guards, body) |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("cls", sf)) |> Result.fail

(*
 * 8.6  Guards
 *)
and guard_sequence_of_sf sf : (guard_sequence_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* empty or non-empty sequence *)
  | Sf.List sf_forms ->
     let%bind forms = sf_forms |> List.map ~f:guard_of_sf |> Result.all |> track ~loc:[%here] in
     GuardSeq forms |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("guard_sequence", sf)) |> Result.fail

and guard_of_sf sf : (guard_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* non-empty sequence *)
  | Sf.List sf_forms when List.length sf_forms > 0 ->
     let%bind forms = sf_forms |> List.map ~f:guard_test_of_sf |> Result.all |> track ~loc:[%here] in
     Guard forms |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("guard", sf)) |> Result.fail

and guard_test_of_sf sf : (guard_test_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* function call *)
  | Sf.Tuple (4, [
                 Sf.Atom "call";
                 Sf.Integer line;
                 sf_name;
                 Sf.List sf_args
             ]) ->
     let%bind name = sf_name |> lit_of_sf |> track ~loc:[%here] in
     let%bind args =  sf_args |> List.map ~f:guard_test_of_sf |> Result.all |> track ~loc:[%here] in
     GuardTestCall (line, name, args) |> return

  (* a map creation *)
  | Sf.Tuple (3, [Sf.Atom "map"; Sf.Integer line; Sf.List sf_assocs]) ->
     let%bind assocs = sf_assocs |> List.map ~f:guard_test_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     GuardTestMapCreation (line, assocs) |> return

  (* a map update *)
  | Sf.Tuple (4, [Sf.Atom "map"; Sf.Integer line; sf_m; Sf.List sf_assocs]) ->
     let%bind m = sf_m |> guard_test_of_sf |> track ~loc:[%here] in
     let%bind assocs = sf_assocs |> List.map ~f:guard_test_assoc_of_sf |> Result.all |> track ~loc:[%here] in
     GuardTestMapUpdate (line, m , assocs) |> return

  (* a binary operator *)
  | Sf.Tuple (5, [Sf.Atom "op"; Sf.Integer line; Sf.Atom op; sf_gt1; sf_gt2]) ->
     let%bind gt1 = sf_gt1 |> guard_test_of_sf |> track ~loc:[%here] in
     let%bind gt2 = sf_gt2 |> guard_test_of_sf |> track ~loc:[%here] in
     GuardTestBinOp (line, op, gt1, gt2) |> return

  (* variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     GuardTestVar (line, id) |> return

  (* atomic literal *)
  | sf_v ->
     let%bind v = sf_v |> lit_of_sf |> track ~loc:[%here] in
     GuardTestLit v |> return

and guard_test_assoc_of_sf sf : (guard_test_assoc_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* an association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_assoc"; Sf.Integer line; sf_k; sf_v]) ->
     let%bind k = sf_k |> guard_test_of_sf |> track ~loc:[%here]in
     let%bind v = sf_v |> guard_test_of_sf |> track ~loc:[%here] in
     GuardTestAssoc (line, k, v) |> return

  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; sf_k; sf_v]) ->
     let%bind k = sf_k |> guard_test_of_sf |> track ~loc:[%here] in
     let%bind v = sf_v |> guard_test_of_sf |> track ~loc:[%here]in
     GuardTestAssocExact (line, k, v) |> return

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
                  Sf.List [sf_a; sf_t]]) ->
     let%bind a = sf_a |> type_of_sf |> track ~loc:[%here] in
     let%bind t = sf_t |> type_of_sf |> track ~loc:[%here] in
     TyAnn (line, a, t) |> return

  (* product type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "product";
                  Sf.List sf_args]) ->
     let%bind args =
       sf_args |> List.map ~f:type_of_sf |> Result.all |> track ~loc:[%here]
     in
     TyProduct (line, args) |> return

  (* predefined (or built-in) type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom n;
                  Sf.List sf_args]) ->
     let%bind args =
       sf_args |> List.map ~f:type_of_sf |> Result.all |> track ~loc:[%here]
     in
     TyPredef (line, n, args) |> return

  (* type variable *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     TyVar (line, id) |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("type", sf)) |> Result.fail

and fun_type_of_sf sf : (type_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  (* constrained function type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "bounded_fun";
                  Sf.List [sf_fun_ty; sf_cont]
             ]) ->
     let%bind fun_ty = sf_fun_ty |> type_of_sf |> track ~loc:[%here] in
     let%bind cont = sf_cont |> type_fun_cont_of_sf |> track ~loc:[%here] in
     TyContFun (line, fun_ty, cont) |> return

  (* function type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "fun";
                  Sf.List [sf_params; sf_ret]]) ->
     let%bind params = sf_params |> type_of_sf |> track ~loc:[%here] in
     let%bind ret = sf_ret |> type_of_sf |> track ~loc:[%here] in
     TyFun (line, params, ret) |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("fun_type", sf)) |> Result.fail

and type_fun_cont_of_sf sf : (type_func_cont_t, err_t) Result.t =
  let open Result.Let_syntax in
  match sf with
  | Sf.List sf_constraints ->
     let%bind constraints =
       sf_constraints |> List.map ~f:type_fun_cont_of_sf |> Result.all |> track ~loc:[%here]
     in
     TyCont constraints |> return

  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "constraint";
                  Sf.List [sf_c; Sf.List [sf_v; sf_t]]
             ]) ->
     let%bind c = sf_c |> type_fun_cont_of_sf |> track ~loc:[%here] in
     let%bind v = sf_v |> type_of_sf |> track ~loc:[%here] in
     let%bind t = sf_t |> type_of_sf |> track ~loc:[%here] in
     TyContRel (line, c, v, t) |> return

  | Sf.Tuple (3, [Sf.Atom "atom"; Sf.Integer line; Sf.Atom "is_subtype"]) ->
     TyContIsSubType line |> return

  | _ ->
     Err.create ~loc:[%here] (Err.Not_supported_absform ("type_fun_cont", sf)) |> Result.fail

(**)
let of_etf etf : (t, err_t) Result.t =
  etf |> Sf.of_etf |> of_sf
