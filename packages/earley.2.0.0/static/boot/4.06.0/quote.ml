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
let dummy_pexp = (exp_ident Location.none "$Antiquotation$").pexp_desc
let dummy_ppat = (pat_ident Location.none "$Antiquotation$").ppat_desc
let dummy_ptyp = Obj.magic (Some None)
let dummy_pcty = Obj.magic (Some None)
let dummy_pctf = Obj.magic (Some None)
let dummy_pcl = Obj.magic (Some None)
let dummy_pcf = Obj.magic (Some None)
let dummy_pmty = Obj.magic (Some None)
let dummy_pmod = Obj.magic (Some None)
let dummy_loc d = d
let dummy_psig =
  Psig_open
    {
      popen_lid = (id_loc (Lident "$Antiquotation$") Location.none);
      popen_override = Fresh;
      popen_loc = Location.none;
      popen_attributes = []
    }
let dummy_pstr =
  Pstr_open
    {
      popen_lid = (id_loc (Lident "$Antiquotation$") Location.none);
      popen_override = Fresh;
      popen_loc = Location.none;
      popen_attributes = []
    }
let anti_table =
  (Hashtbl.create 101 : (Location.t, quotation -> expression) Hashtbl.t)
let string_anti_table = (Hashtbl.create 101 : (string, expression) Hashtbl.t)
let make_antiquotation loc =
  let open Lexing in
    let open Location in
      let f pos = { pos with pos_fname = ("$" ^ (pos.pos_fname ^ "$")) } in
      { loc with loc_start = (f loc.loc_start); loc_end = (f loc.loc_end) }
let make_list_antiquotation loc qtyp f =
  let loc = make_antiquotation loc in
  Hashtbl.add anti_table loc f;
  (let rec l = (Obj.magic loc) :: (Obj.magic qtyp) :: l in l)
let is_antiquotation loc =
  let open Lexing in
    let open Location in
      let s = (loc.loc_start).pos_fname in
      ((String.length s) > 0) && ((s.[0]) = '$')
let is_list_antiquotation l =
  match l with
  | loc::qtyp::l' when l == l' ->
      let loc : Location.t = Obj.magic loc in
      if is_antiquotation loc
      then Some (loc, (Obj.magic qtyp : quotation))
      else None
  | _ -> None
let quote_bool : expression -> Location.t -> bool -> expression =
  fun _ -> Pa_ast.exp_bool
let quote_int : expression -> Location.t -> int -> expression =
  fun _ -> Pa_ast.exp_int
let quote_int32 : expression -> Location.t -> int32 -> expression =
  fun _ -> Pa_ast.exp_int32
let quote_int64 : expression -> Location.t -> int64 -> expression =
  fun _ -> Pa_ast.exp_int64
let quote_nativeint : expression -> Location.t -> nativeint -> expression =
  fun _ -> Pa_ast.exp_nativeint
let quote_char : expression -> Location.t -> char -> expression =
  fun _ -> Pa_ast.exp_char
let anti_string_prefix = "string antiquotation\000"
let quote_string : expression -> Location.t -> string -> expression =
  fun _ ->
    fun loc ->
      fun s ->
        try Hashtbl.find string_anti_table s
        with | Not_found -> Pa_ast.exp_string loc s
let string_antiquotation _loc e =
  let key = anti_string_prefix ^ (Marshal.to_string _loc []) in
  Hashtbl.add string_anti_table key e; key
let quote_option :
  'a .
    (expression -> Location.t -> 'a -> expression) ->
      expression -> Location.t -> 'a option -> expression
  =
  fun qe ->
    fun e_loc ->
      fun _loc ->
        fun eo ->
          let e =
            match eo with | None -> None | Some e -> Some (qe e_loc _loc e) in
          Pa_ast.exp_option _loc e
let quote_list :
  'a .
    (expression -> Location.t -> 'a -> expression) ->
      expression -> Location.t -> 'a list -> expression
  =
  fun qe ->
    fun e_loc ->
      fun _loc ->
        fun el ->
          match is_list_antiquotation el with
          | Some (loc, qtyp) ->
              (try (Hashtbl.find anti_table loc) qtyp
               with
               | Not_found -> failwith "antiquotation not in a quotation")
          | None ->
              let el = List.map (qe e_loc _loc) el in Pa_ast.exp_list _loc el
let quote_tuple : expression -> Location.t -> expression list -> expression =
  fun _ -> Pa_ast.exp_tuple
let quote_apply
  : expression -> Location.t -> Longident.t -> expression list -> expression
  =
  fun _ ->
    fun _loc ->
      fun s ->
        fun l ->
          match l with
          | [] -> Pa_ast.exp_lident _loc s
          | x::[] -> Pa_ast.exp_apply1 _loc (Pa_ast.exp_lident _loc s) x
          | l -> Pa_ast.exp_apply _loc (Pa_ast.exp_lident _loc s) l
let quote_const
  : expression -> Location.t -> Longident.t -> expression list -> expression
  =
  fun _ ->
    fun _loc ->
      fun s ->
        fun l ->
          match l with
          | [] -> Pa_ast.exp_const _loc s None
          | x::[] -> Pa_ast.exp_const _loc s (Some x)
          | l -> Pa_ast.exp_const _loc s (Some (Pa_ast.exp_tuple _loc l))
let lexing s = Ldot ((Lident "Lexing"), s)
let location s = Ldot ((Lident "Location"), s)
let longident s = Ldot ((Lident "Longident"), s)
let parsetree s = Ldot ((Lident "Parsetree"), s)
let asttypes s = Ldot ((Lident "Asttypes"), s)
let pa_ast s = Ldot ((Lident "Pa_ast"), s)
let rec quote_longident
  : expression -> Location.t -> Longident.t -> expression =
  fun e_loc ->
    fun _loc ->
      fun l ->
        match l with
        | Lident s ->
            let s = quote_string e_loc _loc s in
            quote_const e_loc _loc (longident "Lident") [s]
        | Ldot (l, s) ->
            let l = quote_longident e_loc _loc l in
            let s = quote_string e_loc _loc s in
            quote_const e_loc _loc (longident "Ldot") [l; s]
        | Lapply (l, l') ->
            let l = quote_longident e_loc _loc l in
            let l' = quote_longident e_loc _loc l' in
            quote_const e_loc _loc (longident "Lapply") [l; l']
let quote_record
  : expression -> Location.t -> (Longident.t * expression) list -> expression
  = fun _ -> Pa_ast.exp_record
let quote_position
  : expression -> Location.t -> Lexing.position -> expression =
  fun e_loc ->
    fun _loc ->
      fun
        { Lexing.pos_fname = pfn; Lexing.pos_lnum = ln; Lexing.pos_bol = bl;
          Lexing.pos_cnum = pcn }
        ->
        let pfn = quote_string e_loc _loc pfn in
        let ln = quote_int e_loc _loc ln in
        let bl = quote_int e_loc _loc bl in
        let pcn = quote_int e_loc _loc pcn in
        quote_record e_loc _loc
          [((lexing "pos_fname"), pfn);
          ((lexing "pos_lnum"), ln);
          ((lexing "pos_bol"), bl);
          ((lexing "pos_cnum"), pcn)]
let quote_parser_position = ref false
let quote_location_t : expression -> Location.t -> Location.t -> expression =
  fun e_loc ->
    fun _loc ->
      fun
        { Location.loc_start = ls; Location.loc_end = le;
          Location.loc_ghost = g }
        ->
        if !quote_parser_position
        then
          let ls = quote_position e_loc _loc ls in
          let le = quote_position e_loc _loc le in
          let g = quote_bool e_loc _loc g in
          quote_record e_loc _loc
            [((location "loc_start"), ls);
            ((location "loc_end"), le);
            ((location "loc_ghost"), g)]
        else e_loc
(* asttypes.mli *)
let quote_constant e_loc _loc x = match x with
  | Const_int(x) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Const_int")) [quote_int e_loc _loc x]
  | Const_char(x) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Const_char")) [quote_char e_loc _loc x]
  | Const_string(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Const_string")) [ quote_string e_loc _loc x1; (quote_option quote_string) e_loc _loc x2;]
  | Const_float(x) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Const_float")) [quote_string e_loc _loc x]
  | Const_int32(x) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Const_int32")) [quote_int32 e_loc _loc x]
  | Const_int64(x) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Const_int64")) [quote_int64 e_loc _loc x]
  | Const_nativeint(x) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Const_nativeint")) [quote_nativeint e_loc _loc x]

let quote_rec_flag e_loc _loc x = match x with
  | Nonrecursive -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Nonrecursive")) []
  | Recursive -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Recursive")) []

let quote_direction_flag e_loc _loc x = match x with
  | Upto -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Upto")) []
  | Downto -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Downto")) []

let quote_private_flag e_loc _loc x = match x with
  | Private -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Private")) []
  | Public -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Public")) []

let quote_mutable_flag e_loc _loc x = match x with
  | Immutable -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Immutable")) []
  | Mutable -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Mutable")) []

let quote_virtual_flag e_loc _loc x = match x with
  | Virtual -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Virtual")) []
  | Concrete -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Concrete")) []

let quote_override_flag e_loc _loc x = match x with
  | Override -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Override")) []
  | Fresh -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Fresh")) []

let quote_closed_flag e_loc _loc x = match x with
  | Closed -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Closed")) []
  | Open -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Open")) []

let quote_label e_loc _loc x =  quote_string e_loc _loc x
let quote_arg_label e_loc _loc x = match x with
  | Nolabel -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Nolabel")) []
  | Labelled(x) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Labelled")) [quote_string e_loc _loc x]
  | Optional(x) -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Optional")) [quote_string e_loc _loc x]

let quote_loc : 'a. (expression -> Location.t -> 'a -> expression) -> expression -> Location.t -> 'a loc -> expression =
  fun quote_a e_loc _loc r -> if is_antiquotation r.loc then try (Hashtbl.find anti_table r.loc) Quote_loc with Not_found -> failwith "antiquotation not in a quotation" else

    quote_record e_loc _loc [
   ((Ldot(Lident "Asttypes", "txt")), quote_a e_loc _loc r.txt) ;
   ((Ldot(Lident "Asttypes", "loc")), quote_location_t e_loc _loc r.loc) ;
  ]
and loc_antiquotation _loc f dummy_txt = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_id _loc (dummy_txt)

let quote_variance e_loc _loc x = match x with
  | Covariant -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Covariant")) []
  | Contravariant -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Contravariant")) []
  | Invariant -> quote_const e_loc _loc (Ldot(Lident "Asttypes", "Invariant")) []

(* parsetree.mli *)
let quote_constant e_loc _loc x = match x with
  | Pconst_integer(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pconst_integer")) [ quote_string e_loc _loc x1; (quote_option quote_char) e_loc _loc x2;]
  | Pconst_char(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pconst_char")) [quote_char e_loc _loc x]
  | Pconst_string(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pconst_string")) [ quote_string e_loc _loc x1; (quote_option quote_string) e_loc _loc x2;]
  | Pconst_float(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pconst_float")) [ quote_string e_loc _loc x1; (quote_option quote_char) e_loc _loc x2;]

let rec quote_attribute e_loc _loc x =  (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [(quote_loc quote_string) e_loc _loc x1;quote_payload e_loc _loc x2;]) e_loc _loc x
and quote_extension e_loc _loc x =  (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [(quote_loc quote_string) e_loc _loc x1;quote_payload e_loc _loc x2;]) e_loc _loc x
and quote_attributes e_loc _loc x =  (quote_list quote_attribute) e_loc _loc x
and quote_payload e_loc _loc x = match x with
  | PStr(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "PStr")) [quote_structure e_loc _loc x]
  | PSig(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "PSig")) [quote_signature e_loc _loc x]
  | PTyp(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "PTyp")) [quote_core_type e_loc _loc x]
  | PPat(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "PPat")) [ quote_pattern e_loc _loc x1; (quote_option quote_expression) e_loc _loc x2;]

and quote_core_type e_loc _loc r = if is_antiquotation r.ptyp_loc then try (Hashtbl.find anti_table r.ptyp_loc) Quote_ptyp with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "ptyp_desc")), quote_core_type_desc e_loc _loc r.ptyp_desc) ;
   ((Ldot(Lident "Parsetree", "ptyp_loc")), quote_location_t e_loc _loc r.ptyp_loc) ;
   ((Ldot(Lident "Parsetree", "ptyp_attributes")), quote_attributes e_loc _loc r.ptyp_attributes) ;
  ]
and ptyp_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_ptyp _loc (dummy_ptyp)

and quote_core_type_desc e_loc _loc x = match x with
  | Ptyp_any -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_any")) []
  | Ptyp_var(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_var")) [quote_string e_loc _loc x]
  | Ptyp_arrow(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_arrow")) [ quote_arg_label e_loc _loc x1; quote_core_type e_loc _loc x2; quote_core_type e_loc _loc x3;]
  | Ptyp_tuple(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_tuple")) [(quote_list quote_core_type) e_loc _loc x]
  | Ptyp_constr(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_constr")) [ (quote_loc quote_longident) e_loc _loc x1; (quote_list quote_core_type) e_loc _loc x2;]
  | Ptyp_object(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_object")) [ (quote_list quote_object_field) e_loc _loc x1; quote_closed_flag e_loc _loc x2;]
  | Ptyp_class(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_class")) [ (quote_loc quote_longident) e_loc _loc x1; (quote_list quote_core_type) e_loc _loc x2;]
  | Ptyp_alias(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_alias")) [ quote_core_type e_loc _loc x1; quote_string e_loc _loc x2;]
  | Ptyp_variant(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_variant")) [ (quote_list quote_row_field) e_loc _loc x1; quote_closed_flag e_loc _loc x2; (quote_option (quote_list quote_label)) e_loc _loc x3;]
  | Ptyp_poly(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_poly")) [ (quote_list (quote_loc quote_string)) e_loc _loc x1; quote_core_type e_loc _loc x2;]
  | Ptyp_package(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_package")) [quote_package_type e_loc _loc x]
  | Ptyp_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptyp_extension")) [quote_extension e_loc _loc x]

and quote_package_type e_loc _loc x =  (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [(quote_loc quote_longident) e_loc _loc x1;(quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [(quote_loc quote_longident) e_loc _loc x1;quote_core_type e_loc _loc x2;])) e_loc _loc x2;]) e_loc _loc x
and quote_row_field e_loc _loc x = match x with
  | Rtag(x1,x2,x3,x4) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Rtag")) [ (quote_loc quote_label) e_loc _loc x1; quote_attributes e_loc _loc x2; quote_bool e_loc _loc x3; (quote_list quote_core_type) e_loc _loc x4;]
  | Rinherit(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Rinherit")) [quote_core_type e_loc _loc x]

and quote_object_field e_loc _loc x = match x with
  | Otag(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Otag")) [ (quote_loc quote_label) e_loc _loc x1; quote_attributes e_loc _loc x2; quote_core_type e_loc _loc x3;]
  | Oinherit(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Oinherit")) [quote_core_type e_loc _loc x]

and quote_pattern e_loc _loc r = if is_antiquotation r.ppat_loc then try (Hashtbl.find anti_table r.ppat_loc) Quote_ppat with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "ppat_desc")), quote_pattern_desc e_loc _loc r.ppat_desc) ;
   ((Ldot(Lident "Parsetree", "ppat_loc")), quote_location_t e_loc _loc r.ppat_loc) ;
   ((Ldot(Lident "Parsetree", "ppat_attributes")), quote_attributes e_loc _loc r.ppat_attributes) ;
  ]
and ppat_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_ppat _loc (dummy_ppat)

and quote_pattern_desc e_loc _loc x = match x with
  | Ppat_any -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_any")) []
  | Ppat_var(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_var")) [(quote_loc quote_string) e_loc _loc x]
  | Ppat_alias(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_alias")) [ quote_pattern e_loc _loc x1; (quote_loc quote_string) e_loc _loc x2;]
  | Ppat_constant(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_constant")) [quote_constant e_loc _loc x]
  | Ppat_interval(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_interval")) [ quote_constant e_loc _loc x1; quote_constant e_loc _loc x2;]
  | Ppat_tuple(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_tuple")) [(quote_list quote_pattern) e_loc _loc x]
  | Ppat_construct(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_construct")) [ (quote_loc quote_longident) e_loc _loc x1; (quote_option quote_pattern) e_loc _loc x2;]
  | Ppat_variant(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_variant")) [ quote_label e_loc _loc x1; (quote_option quote_pattern) e_loc _loc x2;]
  | Ppat_record(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_record")) [ (quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [(quote_loc quote_longident) e_loc _loc x1;quote_pattern e_loc _loc x2;])) e_loc _loc x1; quote_closed_flag e_loc _loc x2;]
  | Ppat_array(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_array")) [(quote_list quote_pattern) e_loc _loc x]
  | Ppat_or(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_or")) [ quote_pattern e_loc _loc x1; quote_pattern e_loc _loc x2;]
  | Ppat_constraint(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_constraint")) [ quote_pattern e_loc _loc x1; quote_core_type e_loc _loc x2;]
  | Ppat_type(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_type")) [(quote_loc quote_longident) e_loc _loc x]
  | Ppat_lazy(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_lazy")) [quote_pattern e_loc _loc x]
  | Ppat_unpack(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_unpack")) [(quote_loc quote_string) e_loc _loc x]
  | Ppat_exception(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_exception")) [quote_pattern e_loc _loc x]
  | Ppat_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_extension")) [quote_extension e_loc _loc x]
  | Ppat_open(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ppat_open")) [ (quote_loc quote_longident) e_loc _loc x1; quote_pattern e_loc _loc x2;]

and quote_expression e_loc _loc r = if is_antiquotation r.pexp_loc then try (Hashtbl.find anti_table r.pexp_loc) Quote_pexp with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pexp_desc")), quote_expression_desc e_loc _loc r.pexp_desc) ;
   ((Ldot(Lident "Parsetree", "pexp_loc")), quote_location_t e_loc _loc r.pexp_loc) ;
   ((Ldot(Lident "Parsetree", "pexp_attributes")), quote_attributes e_loc _loc r.pexp_attributes) ;
  ]
and pexp_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_pexp _loc (dummy_pexp)

and quote_expression_desc e_loc _loc x = match x with
  | Pexp_ident(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_ident")) [(quote_loc quote_longident) e_loc _loc x]
  | Pexp_constant(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_constant")) [quote_constant e_loc _loc x]
  | Pexp_let(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_let")) [ quote_rec_flag e_loc _loc x1; (quote_list quote_value_binding) e_loc _loc x2; quote_expression e_loc _loc x3;]
  | Pexp_function(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_function")) [(quote_list quote_case) e_loc _loc x]
  | Pexp_fun(x1,x2,x3,x4) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_fun")) [ quote_arg_label e_loc _loc x1; (quote_option quote_expression) e_loc _loc x2; quote_pattern e_loc _loc x3; quote_expression e_loc _loc x4;]
  | Pexp_apply(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_apply")) [ quote_expression e_loc _loc x1; (quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [quote_arg_label e_loc _loc x1;quote_expression e_loc _loc x2;])) e_loc _loc x2;]
  | Pexp_match(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_match")) [ quote_expression e_loc _loc x1; (quote_list quote_case) e_loc _loc x2;]
  | Pexp_try(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_try")) [ quote_expression e_loc _loc x1; (quote_list quote_case) e_loc _loc x2;]
  | Pexp_tuple(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_tuple")) [(quote_list quote_expression) e_loc _loc x]
  | Pexp_construct(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_construct")) [ (quote_loc quote_longident) e_loc _loc x1; (quote_option quote_expression) e_loc _loc x2;]
  | Pexp_variant(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_variant")) [ quote_label e_loc _loc x1; (quote_option quote_expression) e_loc _loc x2;]
  | Pexp_record(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_record")) [ (quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [(quote_loc quote_longident) e_loc _loc x1;quote_expression e_loc _loc x2;])) e_loc _loc x1; (quote_option quote_expression) e_loc _loc x2;]
  | Pexp_field(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_field")) [ quote_expression e_loc _loc x1; (quote_loc quote_longident) e_loc _loc x2;]
  | Pexp_setfield(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_setfield")) [ quote_expression e_loc _loc x1; (quote_loc quote_longident) e_loc _loc x2; quote_expression e_loc _loc x3;]
  | Pexp_array(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_array")) [(quote_list quote_expression) e_loc _loc x]
  | Pexp_ifthenelse(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_ifthenelse")) [ quote_expression e_loc _loc x1; quote_expression e_loc _loc x2; (quote_option quote_expression) e_loc _loc x3;]
  | Pexp_sequence(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_sequence")) [ quote_expression e_loc _loc x1; quote_expression e_loc _loc x2;]
  | Pexp_while(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_while")) [ quote_expression e_loc _loc x1; quote_expression e_loc _loc x2;]
  | Pexp_for(x1,x2,x3,x4,x5) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_for")) [ quote_pattern e_loc _loc x1; quote_expression e_loc _loc x2; quote_expression e_loc _loc x3; quote_direction_flag e_loc _loc x4; quote_expression e_loc _loc x5;]
  | Pexp_constraint(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_constraint")) [ quote_expression e_loc _loc x1; quote_core_type e_loc _loc x2;]
  | Pexp_coerce(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_coerce")) [ quote_expression e_loc _loc x1; (quote_option quote_core_type) e_loc _loc x2; quote_core_type e_loc _loc x3;]
  | Pexp_send(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_send")) [ quote_expression e_loc _loc x1; (quote_loc quote_label) e_loc _loc x2;]
  | Pexp_new(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_new")) [(quote_loc quote_longident) e_loc _loc x]
  | Pexp_setinstvar(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_setinstvar")) [ (quote_loc quote_label) e_loc _loc x1; quote_expression e_loc _loc x2;]
  | Pexp_override(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_override")) [(quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [(quote_loc quote_label) e_loc _loc x1;quote_expression e_loc _loc x2;])) e_loc _loc x]
  | Pexp_letmodule(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_letmodule")) [ (quote_loc quote_string) e_loc _loc x1; quote_module_expr e_loc _loc x2; quote_expression e_loc _loc x3;]
  | Pexp_letexception(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_letexception")) [ quote_extension_constructor e_loc _loc x1; quote_expression e_loc _loc x2;]
  | Pexp_assert(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_assert")) [quote_expression e_loc _loc x]
  | Pexp_lazy(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_lazy")) [quote_expression e_loc _loc x]
  | Pexp_poly(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_poly")) [ quote_expression e_loc _loc x1; (quote_option quote_core_type) e_loc _loc x2;]
  | Pexp_object(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_object")) [quote_class_structure e_loc _loc x]
  | Pexp_newtype(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_newtype")) [ (quote_loc quote_string) e_loc _loc x1; quote_expression e_loc _loc x2;]
  | Pexp_pack(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_pack")) [quote_module_expr e_loc _loc x]
  | Pexp_open(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_open")) [ quote_override_flag e_loc _loc x1; (quote_loc quote_longident) e_loc _loc x2; quote_expression e_loc _loc x3;]
  | Pexp_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_extension")) [quote_extension e_loc _loc x]
  | Pexp_unreachable -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pexp_unreachable")) []

and quote_case e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pc_lhs")), quote_pattern e_loc _loc r.pc_lhs) ;
   ((Ldot(Lident "Parsetree", "pc_guard")), (quote_option quote_expression) e_loc _loc r.pc_guard) ;
   ((Ldot(Lident "Parsetree", "pc_rhs")), quote_expression e_loc _loc r.pc_rhs) ;
  ]

and quote_value_description e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pval_name")), (quote_loc quote_string) e_loc _loc r.pval_name) ;
   ((Ldot(Lident "Parsetree", "pval_type")), quote_core_type e_loc _loc r.pval_type) ;
   ((Ldot(Lident "Parsetree", "pval_prim")), (quote_list quote_string) e_loc _loc r.pval_prim) ;
   ((Ldot(Lident "Parsetree", "pval_attributes")), quote_attributes e_loc _loc r.pval_attributes) ;
   ((Ldot(Lident "Parsetree", "pval_loc")), quote_location_t e_loc _loc r.pval_loc) ;
  ]

and quote_type_declaration e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "ptype_name")), (quote_loc quote_string) e_loc _loc r.ptype_name) ;
   ((Ldot(Lident "Parsetree", "ptype_params")), (quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [quote_core_type e_loc _loc x1;quote_variance e_loc _loc x2;])) e_loc _loc r.ptype_params) ;
   ((Ldot(Lident "Parsetree", "ptype_cstrs")), (quote_list (fun e_loc _loc (x1,x2,x3) -> quote_tuple e_loc _loc [quote_core_type e_loc _loc x1;quote_core_type e_loc _loc x2;quote_location_t e_loc _loc x3;])) e_loc _loc r.ptype_cstrs) ;
   ((Ldot(Lident "Parsetree", "ptype_kind")), quote_type_kind e_loc _loc r.ptype_kind) ;
   ((Ldot(Lident "Parsetree", "ptype_private")), quote_private_flag e_loc _loc r.ptype_private) ;
   ((Ldot(Lident "Parsetree", "ptype_manifest")), (quote_option quote_core_type) e_loc _loc r.ptype_manifest) ;
   ((Ldot(Lident "Parsetree", "ptype_attributes")), quote_attributes e_loc _loc r.ptype_attributes) ;
   ((Ldot(Lident "Parsetree", "ptype_loc")), quote_location_t e_loc _loc r.ptype_loc) ;
  ]

and quote_type_kind e_loc _loc x = match x with
  | Ptype_abstract -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptype_abstract")) []
  | Ptype_variant(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptype_variant")) [(quote_list quote_constructor_declaration) e_loc _loc x]
  | Ptype_record(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptype_record")) [(quote_list quote_label_declaration) e_loc _loc x]
  | Ptype_open -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptype_open")) []

and quote_label_declaration e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pld_name")), (quote_loc quote_string) e_loc _loc r.pld_name) ;
   ((Ldot(Lident "Parsetree", "pld_mutable")), quote_mutable_flag e_loc _loc r.pld_mutable) ;
   ((Ldot(Lident "Parsetree", "pld_type")), quote_core_type e_loc _loc r.pld_type) ;
   ((Ldot(Lident "Parsetree", "pld_loc")), quote_location_t e_loc _loc r.pld_loc) ;
   ((Ldot(Lident "Parsetree", "pld_attributes")), quote_attributes e_loc _loc r.pld_attributes) ;
  ]

and quote_constructor_declaration e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pcd_name")), (quote_loc quote_string) e_loc _loc r.pcd_name) ;
   ((Ldot(Lident "Parsetree", "pcd_args")), quote_constructor_arguments e_loc _loc r.pcd_args) ;
   ((Ldot(Lident "Parsetree", "pcd_res")), (quote_option quote_core_type) e_loc _loc r.pcd_res) ;
   ((Ldot(Lident "Parsetree", "pcd_loc")), quote_location_t e_loc _loc r.pcd_loc) ;
   ((Ldot(Lident "Parsetree", "pcd_attributes")), quote_attributes e_loc _loc r.pcd_attributes) ;
  ]

and quote_constructor_arguments e_loc _loc x = match x with
  | Pcstr_tuple(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcstr_tuple")) [(quote_list quote_core_type) e_loc _loc x]
  | Pcstr_record(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcstr_record")) [(quote_list quote_label_declaration) e_loc _loc x]

and quote_type_extension e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "ptyext_path")), (quote_loc quote_longident) e_loc _loc r.ptyext_path) ;
   ((Ldot(Lident "Parsetree", "ptyext_params")), (quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [quote_core_type e_loc _loc x1;quote_variance e_loc _loc x2;])) e_loc _loc r.ptyext_params) ;
   ((Ldot(Lident "Parsetree", "ptyext_constructors")), (quote_list quote_extension_constructor) e_loc _loc r.ptyext_constructors) ;
   ((Ldot(Lident "Parsetree", "ptyext_private")), quote_private_flag e_loc _loc r.ptyext_private) ;
   ((Ldot(Lident "Parsetree", "ptyext_attributes")), quote_attributes e_loc _loc r.ptyext_attributes) ;
  ]

and quote_extension_constructor e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pext_name")), (quote_loc quote_string) e_loc _loc r.pext_name) ;
   ((Ldot(Lident "Parsetree", "pext_kind")), quote_extension_constructor_kind e_loc _loc r.pext_kind) ;
   ((Ldot(Lident "Parsetree", "pext_loc")), quote_location_t e_loc _loc r.pext_loc) ;
   ((Ldot(Lident "Parsetree", "pext_attributes")), quote_attributes e_loc _loc r.pext_attributes) ;
  ]

and quote_extension_constructor_kind e_loc _loc x = match x with
  | Pext_decl(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pext_decl")) [ quote_constructor_arguments e_loc _loc x1; (quote_option quote_core_type) e_loc _loc x2;]
  | Pext_rebind(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pext_rebind")) [(quote_loc quote_longident) e_loc _loc x]

and quote_class_type e_loc _loc r = if is_antiquotation r.pcty_loc then try (Hashtbl.find anti_table r.pcty_loc) Quote_pcty with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pcty_desc")), quote_class_type_desc e_loc _loc r.pcty_desc) ;
   ((Ldot(Lident "Parsetree", "pcty_loc")), quote_location_t e_loc _loc r.pcty_loc) ;
   ((Ldot(Lident "Parsetree", "pcty_attributes")), quote_attributes e_loc _loc r.pcty_attributes) ;
  ]
and pcty_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_pcty _loc (dummy_pcty)

and quote_class_type_desc e_loc _loc x = match x with
  | Pcty_constr(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcty_constr")) [ (quote_loc quote_longident) e_loc _loc x1; (quote_list quote_core_type) e_loc _loc x2;]
  | Pcty_signature(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcty_signature")) [quote_class_signature e_loc _loc x]
  | Pcty_arrow(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcty_arrow")) [ quote_arg_label e_loc _loc x1; quote_core_type e_loc _loc x2; quote_class_type e_loc _loc x3;]
  | Pcty_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcty_extension")) [quote_extension e_loc _loc x]
  | Pcty_open(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcty_open")) [ quote_override_flag e_loc _loc x1; (quote_loc quote_longident) e_loc _loc x2; quote_class_type e_loc _loc x3;]

and quote_class_signature e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pcsig_self")), quote_core_type e_loc _loc r.pcsig_self) ;
   ((Ldot(Lident "Parsetree", "pcsig_fields")), (quote_list quote_class_type_field) e_loc _loc r.pcsig_fields) ;
  ]

and quote_class_type_field e_loc _loc r = if is_antiquotation r.pctf_loc then try (Hashtbl.find anti_table r.pctf_loc) Quote_pctf with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pctf_desc")), quote_class_type_field_desc e_loc _loc r.pctf_desc) ;
   ((Ldot(Lident "Parsetree", "pctf_loc")), quote_location_t e_loc _loc r.pctf_loc) ;
   ((Ldot(Lident "Parsetree", "pctf_attributes")), quote_attributes e_loc _loc r.pctf_attributes) ;
  ]
and pctf_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_pctf _loc (dummy_pctf)

and quote_class_type_field_desc e_loc _loc x = match x with
  | Pctf_inherit(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pctf_inherit")) [quote_class_type e_loc _loc x]
  | Pctf_val(x1,x2,x3,x4) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pctf_val")) [ (quote_loc quote_label) e_loc _loc x1; quote_mutable_flag e_loc _loc x2; quote_virtual_flag e_loc _loc x3; quote_core_type e_loc _loc x4;]
  | Pctf_method(x1,x2,x3,x4) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pctf_method")) [ (quote_loc quote_label) e_loc _loc x1; quote_private_flag e_loc _loc x2; quote_virtual_flag e_loc _loc x3; quote_core_type e_loc _loc x4;]
  | Pctf_constraint(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pctf_constraint")) [ quote_core_type e_loc _loc x1; quote_core_type e_loc _loc x2;]
  | Pctf_attribute(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pctf_attribute")) [quote_attribute e_loc _loc x]
  | Pctf_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pctf_extension")) [quote_extension e_loc _loc x]

and quote_class_infos : 'a. (expression -> Location.t -> 'a -> expression) -> expression -> Location.t -> 'a class_infos -> expression =
  fun quote_a e_loc _loc r -> 
    quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pci_virt")), quote_virtual_flag e_loc _loc r.pci_virt) ;
   ((Ldot(Lident "Parsetree", "pci_params")), (quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [quote_core_type e_loc _loc x1;quote_variance e_loc _loc x2;])) e_loc _loc r.pci_params) ;
   ((Ldot(Lident "Parsetree", "pci_name")), (quote_loc quote_string) e_loc _loc r.pci_name) ;
   ((Ldot(Lident "Parsetree", "pci_expr")), quote_a e_loc _loc r.pci_expr) ;
   ((Ldot(Lident "Parsetree", "pci_loc")), quote_location_t e_loc _loc r.pci_loc) ;
   ((Ldot(Lident "Parsetree", "pci_attributes")), quote_attributes e_loc _loc r.pci_attributes) ;
  ]

and quote_class_description e_loc _loc x =  (quote_class_infos quote_class_type) e_loc _loc x
and quote_class_type_declaration e_loc _loc x =  (quote_class_infos quote_class_type) e_loc _loc x
and quote_class_expr e_loc _loc r = if is_antiquotation r.pcl_loc then try (Hashtbl.find anti_table r.pcl_loc) Quote_pcl with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pcl_desc")), quote_class_expr_desc e_loc _loc r.pcl_desc) ;
   ((Ldot(Lident "Parsetree", "pcl_loc")), quote_location_t e_loc _loc r.pcl_loc) ;
   ((Ldot(Lident "Parsetree", "pcl_attributes")), quote_attributes e_loc _loc r.pcl_attributes) ;
  ]
and pcl_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_pcl _loc (dummy_pcl)

and quote_class_expr_desc e_loc _loc x = match x with
  | Pcl_constr(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcl_constr")) [ (quote_loc quote_longident) e_loc _loc x1; (quote_list quote_core_type) e_loc _loc x2;]
  | Pcl_structure(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcl_structure")) [quote_class_structure e_loc _loc x]
  | Pcl_fun(x1,x2,x3,x4) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcl_fun")) [ quote_arg_label e_loc _loc x1; (quote_option quote_expression) e_loc _loc x2; quote_pattern e_loc _loc x3; quote_class_expr e_loc _loc x4;]
  | Pcl_apply(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcl_apply")) [ quote_class_expr e_loc _loc x1; (quote_list (fun e_loc _loc (x1,x2) -> quote_tuple e_loc _loc [quote_arg_label e_loc _loc x1;quote_expression e_loc _loc x2;])) e_loc _loc x2;]
  | Pcl_let(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcl_let")) [ quote_rec_flag e_loc _loc x1; (quote_list quote_value_binding) e_loc _loc x2; quote_class_expr e_loc _loc x3;]
  | Pcl_constraint(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcl_constraint")) [ quote_class_expr e_loc _loc x1; quote_class_type e_loc _loc x2;]
  | Pcl_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcl_extension")) [quote_extension e_loc _loc x]
  | Pcl_open(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcl_open")) [ quote_override_flag e_loc _loc x1; (quote_loc quote_longident) e_loc _loc x2; quote_class_expr e_loc _loc x3;]

and quote_class_structure e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pcstr_self")), quote_pattern e_loc _loc r.pcstr_self) ;
   ((Ldot(Lident "Parsetree", "pcstr_fields")), (quote_list quote_class_field) e_loc _loc r.pcstr_fields) ;
  ]

and quote_class_field e_loc _loc r = if is_antiquotation r.pcf_loc then try (Hashtbl.find anti_table r.pcf_loc) Quote_pcf with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pcf_desc")), quote_class_field_desc e_loc _loc r.pcf_desc) ;
   ((Ldot(Lident "Parsetree", "pcf_loc")), quote_location_t e_loc _loc r.pcf_loc) ;
   ((Ldot(Lident "Parsetree", "pcf_attributes")), quote_attributes e_loc _loc r.pcf_attributes) ;
  ]
and pcf_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_pcf _loc (dummy_pcf)

and quote_class_field_desc e_loc _loc x = match x with
  | Pcf_inherit(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcf_inherit")) [ quote_override_flag e_loc _loc x1; quote_class_expr e_loc _loc x2; (quote_option (quote_loc quote_string)) e_loc _loc x3;]
  | Pcf_val(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcf_val")) [ (quote_loc quote_label) e_loc _loc x1; quote_mutable_flag e_loc _loc x2; quote_class_field_kind e_loc _loc x3;]
  | Pcf_method(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcf_method")) [ (quote_loc quote_label) e_loc _loc x1; quote_private_flag e_loc _loc x2; quote_class_field_kind e_loc _loc x3;]
  | Pcf_constraint(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcf_constraint")) [ quote_core_type e_loc _loc x1; quote_core_type e_loc _loc x2;]
  | Pcf_initializer(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcf_initializer")) [quote_expression e_loc _loc x]
  | Pcf_attribute(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcf_attribute")) [quote_attribute e_loc _loc x]
  | Pcf_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pcf_extension")) [quote_extension e_loc _loc x]

and quote_class_field_kind e_loc _loc x = match x with
  | Cfk_virtual(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Cfk_virtual")) [quote_core_type e_loc _loc x]
  | Cfk_concrete(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Cfk_concrete")) [ quote_override_flag e_loc _loc x1; quote_expression e_loc _loc x2;]

and quote_class_declaration e_loc _loc x =  (quote_class_infos quote_class_expr) e_loc _loc x
and quote_module_type e_loc _loc r = if is_antiquotation r.pmty_loc then try (Hashtbl.find anti_table r.pmty_loc) Quote_pmty with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pmty_desc")), quote_module_type_desc e_loc _loc r.pmty_desc) ;
   ((Ldot(Lident "Parsetree", "pmty_loc")), quote_location_t e_loc _loc r.pmty_loc) ;
   ((Ldot(Lident "Parsetree", "pmty_attributes")), quote_attributes e_loc _loc r.pmty_attributes) ;
  ]
and pmty_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_pmty _loc (dummy_pmty)

and quote_module_type_desc e_loc _loc x = match x with
  | Pmty_ident(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmty_ident")) [(quote_loc quote_longident) e_loc _loc x]
  | Pmty_signature(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmty_signature")) [quote_signature e_loc _loc x]
  | Pmty_functor(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmty_functor")) [ (quote_loc quote_string) e_loc _loc x1; (quote_option quote_module_type) e_loc _loc x2; quote_module_type e_loc _loc x3;]
  | Pmty_with(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmty_with")) [ quote_module_type e_loc _loc x1; (quote_list quote_with_constraint) e_loc _loc x2;]
  | Pmty_typeof(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmty_typeof")) [quote_module_expr e_loc _loc x]
  | Pmty_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmty_extension")) [quote_extension e_loc _loc x]
  | Pmty_alias(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmty_alias")) [(quote_loc quote_longident) e_loc _loc x]

and quote_signature e_loc _loc x =  (quote_list quote_signature_item) e_loc _loc x
and quote_signature_item e_loc _loc r = if is_antiquotation r.psig_loc then try (Hashtbl.find anti_table r.psig_loc) Quote_psig with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "psig_desc")), quote_signature_item_desc e_loc _loc r.psig_desc) ;
   ((Ldot(Lident "Parsetree", "psig_loc")), quote_location_t e_loc _loc r.psig_loc) ;
  ]
and psig_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_psig _loc (dummy_psig)

and quote_signature_item_desc e_loc _loc x = match x with
  | Psig_value(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_value")) [quote_value_description e_loc _loc x]
  | Psig_type(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_type")) [ quote_rec_flag e_loc _loc x1; (quote_list quote_type_declaration) e_loc _loc x2;]
  | Psig_typext(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_typext")) [quote_type_extension e_loc _loc x]
  | Psig_exception(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_exception")) [quote_extension_constructor e_loc _loc x]
  | Psig_module(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_module")) [quote_module_declaration e_loc _loc x]
  | Psig_recmodule(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_recmodule")) [(quote_list quote_module_declaration) e_loc _loc x]
  | Psig_modtype(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_modtype")) [quote_module_type_declaration e_loc _loc x]
  | Psig_open(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_open")) [quote_open_description e_loc _loc x]
  | Psig_include(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_include")) [quote_include_description e_loc _loc x]
  | Psig_class(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_class")) [(quote_list quote_class_description) e_loc _loc x]
  | Psig_class_type(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_class_type")) [(quote_list quote_class_type_declaration) e_loc _loc x]
  | Psig_attribute(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_attribute")) [quote_attribute e_loc _loc x]
  | Psig_extension(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Psig_extension")) [ quote_extension e_loc _loc x1; quote_attributes e_loc _loc x2;]

and quote_module_declaration e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pmd_name")), (quote_loc quote_string) e_loc _loc r.pmd_name) ;
   ((Ldot(Lident "Parsetree", "pmd_type")), quote_module_type e_loc _loc r.pmd_type) ;
   ((Ldot(Lident "Parsetree", "pmd_attributes")), quote_attributes e_loc _loc r.pmd_attributes) ;
   ((Ldot(Lident "Parsetree", "pmd_loc")), quote_location_t e_loc _loc r.pmd_loc) ;
  ]

and quote_module_type_declaration e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pmtd_name")), (quote_loc quote_string) e_loc _loc r.pmtd_name) ;
   ((Ldot(Lident "Parsetree", "pmtd_type")), (quote_option quote_module_type) e_loc _loc r.pmtd_type) ;
   ((Ldot(Lident "Parsetree", "pmtd_attributes")), quote_attributes e_loc _loc r.pmtd_attributes) ;
   ((Ldot(Lident "Parsetree", "pmtd_loc")), quote_location_t e_loc _loc r.pmtd_loc) ;
  ]

and quote_open_description e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "popen_lid")), (quote_loc quote_longident) e_loc _loc r.popen_lid) ;
   ((Ldot(Lident "Parsetree", "popen_override")), quote_override_flag e_loc _loc r.popen_override) ;
   ((Ldot(Lident "Parsetree", "popen_loc")), quote_location_t e_loc _loc r.popen_loc) ;
   ((Ldot(Lident "Parsetree", "popen_attributes")), quote_attributes e_loc _loc r.popen_attributes) ;
  ]

and quote_include_infos : 'a. (expression -> Location.t -> 'a -> expression) -> expression -> Location.t -> 'a include_infos -> expression =
  fun quote_a e_loc _loc r -> 
    quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pincl_mod")), quote_a e_loc _loc r.pincl_mod) ;
   ((Ldot(Lident "Parsetree", "pincl_loc")), quote_location_t e_loc _loc r.pincl_loc) ;
   ((Ldot(Lident "Parsetree", "pincl_attributes")), quote_attributes e_loc _loc r.pincl_attributes) ;
  ]

and quote_include_description e_loc _loc x =  (quote_include_infos quote_module_type) e_loc _loc x
and quote_include_declaration e_loc _loc x =  (quote_include_infos quote_module_expr) e_loc _loc x
and quote_with_constraint e_loc _loc x = match x with
  | Pwith_type(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pwith_type")) [ (quote_loc quote_longident) e_loc _loc x1; quote_type_declaration e_loc _loc x2;]
  | Pwith_module(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pwith_module")) [ (quote_loc quote_longident) e_loc _loc x1; (quote_loc quote_longident) e_loc _loc x2;]
  | Pwith_typesubst(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pwith_typesubst")) [ (quote_loc quote_longident) e_loc _loc x1; quote_type_declaration e_loc _loc x2;]
  | Pwith_modsubst(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pwith_modsubst")) [ (quote_loc quote_longident) e_loc _loc x1; (quote_loc quote_longident) e_loc _loc x2;]

and quote_module_expr e_loc _loc r = if is_antiquotation r.pmod_loc then try (Hashtbl.find anti_table r.pmod_loc) Quote_pmod with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pmod_desc")), quote_module_expr_desc e_loc _loc r.pmod_desc) ;
   ((Ldot(Lident "Parsetree", "pmod_loc")), quote_location_t e_loc _loc r.pmod_loc) ;
   ((Ldot(Lident "Parsetree", "pmod_attributes")), quote_attributes e_loc _loc r.pmod_attributes) ;
  ]
and pmod_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_pmod _loc (dummy_pmod)

and quote_module_expr_desc e_loc _loc x = match x with
  | Pmod_ident(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmod_ident")) [(quote_loc quote_longident) e_loc _loc x]
  | Pmod_structure(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmod_structure")) [quote_structure e_loc _loc x]
  | Pmod_functor(x1,x2,x3) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmod_functor")) [ (quote_loc quote_string) e_loc _loc x1; (quote_option quote_module_type) e_loc _loc x2; quote_module_expr e_loc _loc x3;]
  | Pmod_apply(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmod_apply")) [ quote_module_expr e_loc _loc x1; quote_module_expr e_loc _loc x2;]
  | Pmod_constraint(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmod_constraint")) [ quote_module_expr e_loc _loc x1; quote_module_type e_loc _loc x2;]
  | Pmod_unpack(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmod_unpack")) [quote_expression e_loc _loc x]
  | Pmod_extension(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pmod_extension")) [quote_extension e_loc _loc x]

and quote_structure e_loc _loc x =  (quote_list quote_structure_item) e_loc _loc x
and quote_structure_item e_loc _loc r = if is_antiquotation r.pstr_loc then try (Hashtbl.find anti_table r.pstr_loc) Quote_pstr with Not_found -> failwith "antiquotation not in a quotation" else
  quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pstr_desc")), quote_structure_item_desc e_loc _loc r.pstr_desc) ;
   ((Ldot(Lident "Parsetree", "pstr_loc")), quote_location_t e_loc _loc r.pstr_loc) ;
  ]
and pstr_antiquotation _loc f = let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; loc_pstr _loc (dummy_pstr)

and quote_structure_item_desc e_loc _loc x = match x with
  | Pstr_eval(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_eval")) [ quote_expression e_loc _loc x1; quote_attributes e_loc _loc x2;]
  | Pstr_value(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_value")) [ quote_rec_flag e_loc _loc x1; (quote_list quote_value_binding) e_loc _loc x2;]
  | Pstr_primitive(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_primitive")) [quote_value_description e_loc _loc x]
  | Pstr_type(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_type")) [ quote_rec_flag e_loc _loc x1; (quote_list quote_type_declaration) e_loc _loc x2;]
  | Pstr_typext(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_typext")) [quote_type_extension e_loc _loc x]
  | Pstr_exception(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_exception")) [quote_extension_constructor e_loc _loc x]
  | Pstr_module(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_module")) [quote_module_binding e_loc _loc x]
  | Pstr_recmodule(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_recmodule")) [(quote_list quote_module_binding) e_loc _loc x]
  | Pstr_modtype(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_modtype")) [quote_module_type_declaration e_loc _loc x]
  | Pstr_open(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_open")) [quote_open_description e_loc _loc x]
  | Pstr_class(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_class")) [(quote_list quote_class_declaration) e_loc _loc x]
  | Pstr_class_type(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_class_type")) [(quote_list quote_class_type_declaration) e_loc _loc x]
  | Pstr_include(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_include")) [quote_include_declaration e_loc _loc x]
  | Pstr_attribute(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_attribute")) [quote_attribute e_loc _loc x]
  | Pstr_extension(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pstr_extension")) [ quote_extension e_loc _loc x1; quote_attributes e_loc _loc x2;]

and quote_value_binding e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pvb_pat")), quote_pattern e_loc _loc r.pvb_pat) ;
   ((Ldot(Lident "Parsetree", "pvb_expr")), quote_expression e_loc _loc r.pvb_expr) ;
   ((Ldot(Lident "Parsetree", "pvb_attributes")), quote_attributes e_loc _loc r.pvb_attributes) ;
   ((Ldot(Lident "Parsetree", "pvb_loc")), quote_location_t e_loc _loc r.pvb_loc) ;
  ]

and quote_module_binding e_loc _loc r =   quote_record e_loc _loc [
   ((Ldot(Lident "Parsetree", "pmb_name")), (quote_loc quote_string) e_loc _loc r.pmb_name) ;
   ((Ldot(Lident "Parsetree", "pmb_expr")), quote_module_expr e_loc _loc r.pmb_expr) ;
   ((Ldot(Lident "Parsetree", "pmb_attributes")), quote_attributes e_loc _loc r.pmb_attributes) ;
   ((Ldot(Lident "Parsetree", "pmb_loc")), quote_location_t e_loc _loc r.pmb_loc) ;
  ]

let rec quote_toplevel_phrase e_loc _loc x = match x with
  | Ptop_def(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptop_def")) [quote_structure e_loc _loc x]
  | Ptop_dir(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Ptop_dir")) [ quote_string e_loc _loc x1; quote_directive_argument e_loc _loc x2;]

and quote_directive_argument e_loc _loc x = match x with
  | Pdir_none -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pdir_none")) []
  | Pdir_string(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pdir_string")) [quote_string e_loc _loc x]
  | Pdir_int(x1,x2) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pdir_int")) [ quote_string e_loc _loc x1; (quote_option quote_char) e_loc _loc x2;]
  | Pdir_ident(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pdir_ident")) [quote_longident e_loc _loc x]
  | Pdir_bool(x) -> quote_const e_loc _loc (Ldot(Lident "Parsetree", "Pdir_bool")) [quote_bool e_loc _loc x]

