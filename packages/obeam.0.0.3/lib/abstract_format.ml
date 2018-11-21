(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Sf = Simple_term_format
module Z = Aux.Z

let raise_unknown_error form sf =
  failwith (Printf.sprintf "%s: unknown / %s" form (Sf.show sf))

(* line number *)
type line_t = int
[@@deriving show]

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

  | TyContFun of line_t * type_t * type_t
  | TyFun of line_t * type_t * type_t

  | TyCont of type_t list
  | TyContRel of line_t * type_t * type_t * type_t
  | TyContIsSubType of line_t
[@@deriving show]

(*
 * Entry
 *)
let rec of_sf sf =
  match sf with
  | Sf.Tuple (2, [Sf.Atom "raw_abstract_v1"; forms]) ->
     AbstractCode (forms |> form_of_sf)
  (* is it suitable here? *)
  | Sf.Tuple (3, [Sf.Atom "debug_info_v1";
                  Sf.Atom "erl_abstract_code";
                  Sf.Tuple (2, [forms; _options])]) ->
     AbstractCode (forms |> form_of_sf)
  | _ ->
     raise_unknown_error "root" sf

(*
 * 8.1  Module Declarations and Forms
 *)
and form_of_sf sf =
  match sf with
  (* module declaration *)
  | Sf.List forms ->
     ModDecl (forms |> List.map form_of_sf)

  (* attribute -export *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "export";
                  Sf.List name_and_arity_list
                 ]) ->
     AttrExport (line, name_and_arity_list |> List.map name_and_arity_of_sf)

  (* attribute -export_type *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "export_type";
                  Sf.List name_and_arity_list
                 ]) ->
     AttrExportType (line, name_and_arity_list |> List.map name_and_arity_of_sf)

  (* attribute -import *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "import";
                  Sf.List name_and_arity_list
                 ]) ->
     AttrImport (line, name_and_arity_list |> List.map name_and_arity_of_sf)

  (* attribute -module *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "module";
                  Sf.Atom module_name
                 ]) ->
     AttrMod (line, module_name)

  (* attribute -file *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "file";
                  Sf.Tuple (2, [Sf.String file; Sf.Integer file_line])
                 ]) ->
     AttrFile (line, file, file_line)

  (* function declaration *)
  | Sf.Tuple (5, [Sf.Atom "function";
                  Sf.Integer line;
                  Sf.Atom name;
                  Sf.Integer arity;
                  Sf.List f_clauses]) ->
     DeclFun (line, name, arity, f_clauses |> List.map (cls_of_sf ~in_function:true))

  (* function specification *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "spec";
                  Sf.Tuple (2, [Sf.Tuple (2, [Sf.Atom name; Sf.Integer arity]);
                                Sf.List specs])
                 ]) ->
     SpecFun (line, None, name, arity, specs |> List.map fun_type_of_sf)

  (* function specification(Mod) *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "spec";
                  Sf.Tuple (2, [Sf.Tuple (3, [Sf.Atom m; Sf.Atom name; Sf.Integer arity]);
                                Sf.List specs])
                 ]) ->
     SpecFun (line, Some m, name, arity, specs |> List.map fun_type_of_sf)

  (* record declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "record";
                  Sf.Tuple (2, [Sf.Atom name; Sf.List record_fields])
                 ]) ->
    DeclRecord (line, record_fields |> List.map record_field_of_sf)

  (* type declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "type";
                  Sf.Tuple (3, [Sf.Atom name;
                                t;
                                Sf.List tvars
                               ]);
                 ]) ->
    DeclType (line, name, tvars |> List.map tvar_of_sf, type_of_sf t)

  (* opaque type declaration *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "opaque";
                  Sf.Tuple (3, [Sf.Atom name;
                                t;
                                Sf.List tvars
                               ]);
                 ]) ->
    DeclOpaqueType (line, name, tvars |> List.map tvar_of_sf, type_of_sf t)

  (* wild attribute *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom attr;
                  term]) ->
     AttrWild (line, attr, term)

  (* eof *)
  | Sf.Tuple (2, [Sf.Atom "eof"; Sf.Integer line]) ->
     FormEof

  | _ ->
     raise_unknown_error "form" sf

and name_and_arity_of_sf sf =
  match sf with
  | Sf.Tuple (2, [Sf.Atom name; Sf.Integer arity]) ->
     (name, arity)

  | _ ->
     raise_unknown_error "name and arity" sf

and record_field_of_sf sf =
  match sf with
  | Sf.Tuple (3, [Sf.Atom "record_field";
                  Sf.Integer line;
                  Sf.Tuple (3, [Sf.Atom "atom"; _; Sf.Atom field])
                 ]) ->
    (line, field, None, None)

  | Sf.Tuple (4, [Sf.Atom "record_field";
                  Sf.Integer line;
                  Sf.Tuple (3, [Sf.Atom "atom"; _; Sf.Atom field]);
                  e
                 ]) ->
    (line, field, Some (expr_of_sf e), None)

  | Sf.Tuple (3, [Sf.Atom "typed_record_field";
                  Sf.Tuple (3, [Sf.Atom "record_field";
                                Sf.Integer line;
                                Sf.Tuple (3, [Sf.Atom "atom"; _; Sf.Atom field])
                               ]);
                  t
                 ]) ->
    (line, field, None, Some (type_of_sf t))

  | Sf.Tuple (3, [Sf.Atom "typed_record_field";
                  Sf.Tuple (4, [Sf.Atom "record_field";
                                Sf.Integer line;
                                Sf.Tuple (3, [Sf.Atom "atom"; _; Sf.Atom field]);
                                e
                               ]);
                  t
                 ]) ->
    (line, field, Some (expr_of_sf e), Some (type_of_sf t))

  | _ ->
     raise_unknown_error "record_field" sf

and tvar_of_sf sf =
  match sf with
  | Sf.Tuple (3, [Sf.Atom "var";
                  Sf.Integer line;
                  Sf.Atom tvar
                 ]) ->
    (line, tvar)

  | _ ->
     raise_unknown_error "type variable" sf

(*
 * 8.2  Atomic Literals
 *)
and lit_of_sf sf =
  match sf with
  | Sf.Tuple (3, [Sf.Atom "atom"; Sf.Integer line; Sf.Atom v]) ->
     LitAtom (line, v)

  | Sf.Tuple (3, [Sf.Atom "char"; Sf.Integer line; _]) ->
     failwith "TODO"

  | Sf.Tuple (3, [Sf.Atom "float"; Sf.Integer line; _]) ->
     failwith "TODO"

  | Sf.Tuple (3, [Sf.Atom "integer"; Sf.Integer line; Sf.Integer v]) ->
     LitInteger (line, v)

  | Sf.Tuple (3, [Sf.Atom "integer"; Sf.Integer line; Sf.BigInt v]) ->
     LitBigInt (line, v)

  | Sf.Tuple (3, [Sf.Atom "string"; Sf.Integer line; Sf.String v]) ->
     LitString (line, v)

  | _ ->
     raise_unknown_error "literal" sf

(*
 * 8.3  Patterns
 *)
and pat_of_sf sf =
  match sf with
  (* a map pattern *)
  | Sf.Tuple (3, [Sf.Atom "map"; Sf.Integer line; Sf.List assocs]) ->
     PatMap (line, assocs |> List.map pat_assoc_of_sf)

  (* a variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom "_"]) ->
     PatUniversal line

  (* a variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     PatVar (line, id)

  (* atomic literal *)
  | v ->
     PatLit (v |> lit_of_sf)

and pat_assoc_of_sf sf =
  match sf with
  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; k; v]) ->
     PatAssocExact (line, k |> pat_of_sf, v |> pat_of_sf)

  | _ ->
     raise_unknown_error "association" sf

(*
 * 8.4  Expressions
 *)
and expr_of_sf sf =
  match sf with
  | Sf.List es ->
     ExprBody (es |> List.map expr_of_sf)

  (* a case expression *)
  | Sf.Tuple (4, [Sf.Atom "case"; Sf.Integer line; e; Sf.List clauses]) ->
     ExprCase (line, e |> expr_of_sf, clauses |> List.map cls_of_sf)

  (* a map creation *)
  | Sf.Tuple (3, [Sf.Atom "map";
                  Sf.Integer line;
                  Sf.List assocs]) ->
     ExprMapCreation (line, assocs |> List.map expr_assoc_of_sf)

  (* a map update *)
  | Sf.Tuple (4, [Sf.Atom "map";
                  Sf.Integer line;
                  m;
                  Sf.List assocs]) ->
     ExprMapUpdate (line, m |> expr_of_sf, assocs |> List.map expr_assoc_of_sf)

  (* an operator expression binary *)
  | Sf.Tuple (5, [Sf.Atom "op";
                  Sf.Integer line;
                  Sf.Atom op;
                  p1;
                  p2]) ->
     ExprBinOp (line, op, p1 |> expr_of_sf, p2 |> expr_of_sf)

  (* a variable *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     ExprVar (line, id)

  (* atomic literal *)
  | v ->
     ExprLit (v |> lit_of_sf)

and expr_assoc_of_sf sf =
  match sf with
  (* an association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_assoc"; Sf.Integer line; k; v]) ->
     ExprAssoc (line, k |> expr_of_sf, v |> expr_of_sf)

  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; k; v]) ->
     ExprAssocExact (line, k |> expr_of_sf, v |> expr_of_sf)

  | _ ->
     raise_unknown_error "association" sf

(*
 * 8.5  Clauses
 *)
and cls_of_sf ?(in_function=false) sf =
  match sf, in_function with
  (* case clause P -> B *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List [pattern];
                 Sf.List [];
                 body
               ]), false ->
     ClsCase (line, pattern |> pat_of_sf, None, body |> expr_of_sf)

  (* case clause P -> B when Gs *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List [pattern];
                 guards;
                 body
               ]), false ->
     ClsCase (line,
              pattern |> pat_of_sf,
              Some (guards |> guard_sequence_of_sf),
              body |> expr_of_sf)

  (* function clause ( Ps ) -> B *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List patterns;
                 Sf.List [];
                 body
               ]), true ->
     ClsFun (line, patterns |> List.map pat_of_sf, None, body |> expr_of_sf)

  (* function clause ( Ps ) when Gs -> B *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List patterns;
                 guards;
                 body
               ]), true ->
     ClsFun (line,
             patterns |> List.map pat_of_sf,
             Some (guards |> guard_sequence_of_sf),
             body |> expr_of_sf)

  | _ ->
     raise_unknown_error "cls" sf

(*
 * 8.6  Guards
 *)
and guard_sequence_of_sf sf =
  match sf with
  (* empty or non-empty sequence *)
  | Sf.List forms ->
     GuardSeq (forms |> List.map guard_of_sf)

  | _ ->
     raise_unknown_error "guard_sequence" sf

and guard_of_sf sf =
  match sf with
  (* non-empty sequence *)
  | Sf.List forms when List.length forms > 0 ->
     Guard (forms |> List.map guard_test_of_sf)

  | _ ->
     raise_unknown_error "guard" sf

and guard_test_of_sf sf =
  match sf with
  (* function call *)
  | Sf.Tuple (4, [
                 Sf.Atom "call";
                 Sf.Integer line;
                 name;
                 Sf.List args
               ]) ->
     GuardTestCall (line, name |> lit_of_sf, args |> List.map guard_test_of_sf)

  (* a map creation *)
  | Sf.Tuple (3, [Sf.Atom "map"; Sf.Integer line; Sf.List assocs]) ->
     GuardTestMapCreation (line, assocs |> List.map guard_test_assoc_of_sf)

  (* a map update *)
  | Sf.Tuple (4, [Sf.Atom "map"; Sf.Integer line; m; Sf.List assocs]) ->
     GuardTestMapUpdate (line, m |> guard_test_of_sf, assocs |> List.map guard_test_assoc_of_sf)

  (* a binary operator *)
  | Sf.Tuple (5, [Sf.Atom "op"; Sf.Integer line; Sf.Atom op; gt1; gt2]) ->
     GuardTestBinOp (line, op, gt1 |> guard_test_of_sf, gt2 |> guard_test_of_sf)

  (* variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     GuardTestVar (line, id)

  (* atomic literal *)
  | v ->
     GuardTestLit (v |> lit_of_sf)

and guard_test_assoc_of_sf sf =
  match sf with
  (* an association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_assoc"; Sf.Integer line; k; v]) ->
     GuardTestAssoc (line, k |> guard_test_of_sf, v |> guard_test_of_sf)

  (* an exact association *)
  | Sf.Tuple (4, [Sf.Atom "map_field_exact"; Sf.Integer line; k; v]) ->
     GuardTestAssocExact (line, k |> guard_test_of_sf, v |> guard_test_of_sf)

  | _ ->
     raise_unknown_error "association" sf

(*
 * 8.7  Types
 *)
and type_of_sf sf =
  match sf with
  (* annotated type *)
  | Sf.Tuple (3, [Sf.Atom "ann_type";
                  Sf.Integer line;
                  Sf.List [a; t]]) ->
     TyAnn (line, a |> type_of_sf, t |> type_of_sf)

  (* product type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "product";
                  Sf.List args]) ->
     TyProduct (line, args |> List.map type_of_sf)

  (* predefined (or built-in) type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom n;
                  Sf.List args]) ->
     TyPredef (line, n, args |> List.map type_of_sf)

  (* type variable *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     TyVar (line, id)

  | _ ->
     raise_unknown_error "type" sf

and fun_type_of_sf sf =
  match sf with
  (* constrained function type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "bounded_fun";
                  Sf.List [fun_ty; cont]
                 ]) ->
     TyContFun (line, fun_ty |> type_of_sf, cont |> cont_of_sf)

  (* function type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "fun";
                  Sf.List [params; ret]]) ->
     TyFun (line, params |> type_of_sf, ret |> type_of_sf)

  | _ ->
     raise_unknown_error "fun_type" sf

and cont_of_sf sf =
  match sf with
  | Sf.List constraints ->
     TyCont (constraints |> List.map cont_of_sf)

  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "constraint";
                  Sf.List [c; Sf.List [v; t]]
                 ]) ->
     TyContRel (line, c |> cont_of_sf, v |> type_of_sf, t |> type_of_sf)

  | Sf.Tuple (3, [Sf.Atom "atom"; Sf.Integer line; Sf.Atom "is_subtype"]) ->
     TyContIsSubType line

  | _ ->
     raise_unknown_error "cont_type" sf

(**)
let of_etf etf =
  etf |> Sf.of_etf |> of_sf
