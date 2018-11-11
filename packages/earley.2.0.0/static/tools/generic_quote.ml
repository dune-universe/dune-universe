open Asttypes
open Parsetree
open Longident
open Pa_ast

let loc_ptyp = loc_typ
let loc_ppat = loc_pat
let loc_pexp = loc_expr
let loc_pcty = pcty_loc
let loc_pctf = pctf_loc
let loc_pmty = mtyp_loc
let loc_pmod = mexpr_loc
let loc_psig = loc_sig
let loc_pstr = loc_str

type quotation =
  | Quote_pexp
  | Quote_ppat
  | Quote_ptyp
  | Quote_pcty
  | Quote_pctf
  | Quote_pcl
  | Quote_pcf
  | Quote_pmty
  | Quote_psig
  | Quote_pmod
  | Quote_pstr
  | Quote_loc
  | Quote_cases

let dummy_pexp   = (exp_ident Location.none "$Antiquotation$").pexp_desc
let dummy_ppat   = (pat_ident Location.none "$Antiquotation$").ppat_desc
let dummy_ptyp   = Obj.magic (Some None)
let dummy_pcty   = Obj.magic (Some None)
let dummy_pctf   = Obj.magic (Some None)
let dummy_pcl    = Obj.magic (Some None)
let dummy_pcf    = Obj.magic (Some None)
let dummy_pmty   = Obj.magic (Some None)
let dummy_pmod   = Obj.magic (Some None)
let dummy_loc d  = d
let dummy_psig   = Psig_open { popen_lid = id_loc (Lident "$Antiquotation$")  Location.none;
			       popen_override = Fresh;
			       popen_loc = Location.none;
			       popen_attributes = [] }
let dummy_pstr   = Pstr_open { popen_lid = id_loc (Lident "$Antiquotation$")  Location.none;
			       popen_override = Fresh;
			       popen_loc = Location.none;
			       popen_attributes = [] }

let anti_table = (Hashtbl.create 101 : (Location.t, quotation -> expression) Hashtbl.t)
let string_anti_table = (Hashtbl.create 101 : (string,expression) Hashtbl.t)

let make_antiquotation loc =
  let open Lexing in
  let open Location in
  let f pos = { pos with pos_fname = "$"^pos.pos_fname^"$" } in
  { loc with loc_start = f loc.loc_start; loc_end = f loc.loc_end }

let make_list_antiquotation loc qtyp f =
  let loc = make_antiquotation loc in
  Hashtbl.add anti_table loc f;
  let rec l = Obj.magic loc :: Obj.magic qtyp :: l in
  l

let is_antiquotation loc =
  let open Lexing in
  let open Location in
  let s = loc.loc_start.pos_fname in
  String.length s > 0 && s.[0] = '$'

let is_list_antiquotation l =
  match l with
  | loc::qtyp::l' when l == l' ->
     let loc : Location.t = Obj.magic loc in
     if is_antiquotation loc then Some(loc, (Obj.magic qtyp : quotation))
     else None
  | _ -> None

(* Generic functions *)
let quote_bool : expression -> Location.t -> bool -> expression = fun _ ->
  Pa_ast.exp_bool

let quote_int : expression -> Location.t -> int -> expression = fun _ ->
  Pa_ast.exp_int

let quote_int32 : expression -> Location.t -> int32 -> expression = fun _ ->
  Pa_ast.exp_int32

let quote_int64 : expression -> Location.t -> int64 -> expression = fun _ ->
  Pa_ast.exp_int64

let quote_nativeint : expression -> Location.t -> nativeint -> expression = fun _ ->
  Pa_ast.exp_nativeint

let quote_char : expression -> Location.t -> char -> expression = fun _ ->
  Pa_ast.exp_char

let anti_string_prefix = "string antiquotation\000"
let quote_string : expression -> Location.t -> string -> expression = fun _ loc s ->
  try Hashtbl.find string_anti_table s
  with Not_found -> Pa_ast.exp_string loc s
let string_antiquotation _loc e =
  let key = anti_string_prefix ^ Marshal.to_string _loc [] in
  Hashtbl.add string_anti_table key e;
  key

let quote_option : 'a. (expression -> Location.t -> 'a -> expression) -> expression -> Location.t -> 'a option -> expression =
  fun qe e_loc _loc eo ->
    let e =
      match eo with
      | None   -> None
      | Some e -> Some (qe e_loc _loc e)
    in Pa_ast.exp_option _loc e

let quote_list : 'a. (expression -> Location.t -> 'a -> expression) -> expression -> Location.t -> 'a list -> expression =
  fun qe e_loc _loc el ->
    match is_list_antiquotation el with
    | Some(loc,qtyp) ->
       (try (Hashtbl.find anti_table loc) qtyp with Not_found ->
         failwith "antiquotation not in a quotation")
    | None ->
       let el = List.map (qe e_loc _loc) el in
       Pa_ast.exp_list _loc el

let quote_tuple : expression -> Location.t -> expression list -> expression = fun _ ->
  Pa_ast.exp_tuple

let quote_apply : expression -> Location.t -> Longident.t -> expression list -> expression =
  (fun _ _loc s l ->
    match l with [] -> Pa_ast.exp_lident _loc s
               | [x] -> Pa_ast.exp_apply1 _loc (Pa_ast.exp_lident _loc s) x
	       | l -> Pa_ast.exp_apply _loc (Pa_ast.exp_lident _loc s) l)

let quote_const : expression -> Location.t -> Longident.t -> expression list -> expression =
  (fun _ _loc s l ->
    match l with [] -> Pa_ast.exp_const _loc s None
                | [x] -> Pa_ast.exp_const _loc s (Some x)
                | l -> Pa_ast.exp_const _loc s (Some (Pa_ast.exp_tuple _loc l)))

let lexing s = Ldot(Lident "Lexing", s)
let location s = Ldot(Lident "Location", s)
let longident s = Ldot(Lident "Longident", s)
let parsetree s = Ldot(Lident "Parsetree", s)
let asttypes s = Ldot(Lident "Asttypes", s)
let pa_ast s = Ldot(Lident "Pa_ast", s)

let rec quote_longident : expression -> Location.t -> Longident.t -> expression =
  fun e_loc _loc l ->
    match l with
    | Lident s       -> let s = quote_string e_loc _loc s in
                        quote_const e_loc _loc (longident "Lident") [s]
    | Ldot (l, s)    -> let l = quote_longident e_loc _loc l in
                        let s = quote_string e_loc _loc s in
                        quote_const e_loc _loc (longident "Ldot") [l; s]
    | Lapply (l, l') -> let l = quote_longident e_loc _loc l in
                        let l' = quote_longident e_loc _loc l' in
                        quote_const e_loc _loc (longident "Lapply") [l; l']

let quote_record : expression -> Location.t -> (Longident.t * expression) list -> expression = fun _ ->
  Pa_ast.exp_record

let quote_position : expression -> Location.t -> Lexing.position -> expression =
  (fun e_loc _loc {Lexing.pos_fname = pfn; Lexing.pos_lnum = ln; Lexing.pos_bol = bl; Lexing.pos_cnum = pcn} ->
    let pfn = quote_string e_loc _loc pfn in
    let ln  = quote_int e_loc _loc ln in
    let bl  = quote_int e_loc _loc bl in
    let pcn = quote_int e_loc _loc pcn in
    quote_record e_loc _loc
      [(lexing "pos_fname",pfn); (lexing "pos_lnum",ln); (lexing "pos_bol",bl); (lexing "pos_cnum",pcn)])

(* forget the original location of the quotation *)

let quote_parser_position = ref false

let quote_location_t : expression -> Location.t -> Location.t -> expression =
  (fun e_loc _loc {Location.loc_start = ls; Location.loc_end = le; Location.loc_ghost = g} ->
    if !quote_parser_position then
      begin
	let ls = quote_position e_loc _loc ls in
	let le = quote_position e_loc _loc le in
	let g  = quote_bool e_loc _loc g in
	quote_record e_loc _loc [(location "loc_start",ls); (location "loc_end",le); (location "loc_ghost",g)]
      end
    else e_loc)
