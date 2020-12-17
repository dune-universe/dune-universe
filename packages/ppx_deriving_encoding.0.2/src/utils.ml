open Ppxlib
open Ast_builder.Default

let verbose = match Sys.getenv_opt "PPX_ENCODING_DEBUG" with
  | None | Some "0" | Some "false" | Some "no" -> 0
  | Some s ->
    match s with
    | "true" -> 1
    | s -> match int_of_string_opt s with
      | Some i -> i
      | None -> 0

let debug ?(v=1) ?(force=false) fmt =
  if force || verbose >= v then Format.ksprintf (fun s -> Format.eprintf "%s@." s) fmt
  else Printf.ifprintf () fmt

let raise_error ~loc s = Location.raise_errorf ~loc s
let enc_name name = if name = "t" then "enc" else name ^ "_enc"
let enc_mod s = "Json_encoding." ^ s
let enc_var ~loc s = evar ~loc (enc_mod s)
let enc_apply ~loc s l = eapply ~loc (enc_var ~loc s) l

let acc_map f l =
  let encs, accs = List.split @@ List.map f l in
  encs, List.flatten accs

let pexp_fun ~loc p e = pexp_fun ~loc Nolabel None p e
let llid ~loc s = {txt=Longident.parse s; loc}
let esome ~loc e =
  pexp_construct ~loc (llid ~loc "Some") (Some (e ~loc))
let psome ~loc e =
  ppat_construct ~loc (llid ~loc "Some") (Some (e ~loc))
let enone ~loc =
  pexp_construct ~loc (llid ~loc "None") None

let conv1 ~loc construct destruct enc =
  enc_apply ~loc "conv" [
    pexp_fun ~loc (pvar ~loc "x") (construct (evar ~loc "x"));
    pexp_fun ~loc (pvar ~loc "x") (destruct (evar ~loc "x"));
    enc ]

let rec add_params_fun ~loc expr = function
  | [] -> expr
  | {ptyp_desc = Ptyp_var x; _} :: t ->
    pexp_fun ~loc
      (ppat_constraint ~loc (pvar ~loc ("_" ^ enc_name x))
         (ptyp_constr ~loc (llid ~loc (enc_mod "encoding")) [ptyp_var ~loc x]))
      (add_params_fun expr ~loc t)
  | _ :: t -> add_params_fun ~loc expr t

let rec add_params_fun_sig ~loc typ = function
  | [] -> typ
  | {ptyp_desc = Ptyp_var x; _} :: t ->
    ptyp_arrow ~loc Nolabel
      (ptyp_constr ~loc (llid ~loc (enc_mod "encoding")) [ptyp_var ~loc x])
      (add_params_fun_sig typ ~loc t)
  | _ :: t -> add_params_fun_sig ~loc typ t

let param_names params =
  List.rev @@ List.fold_left (fun acc (p, _) -> match p.ptyp_desc with
      | Ptyp_var x -> x :: acc
      | _ -> acc) [] params

let get_expr_attr = function
  | PStr [{pstr_desc = Pstr_eval (e, _); _}] -> Some e
  | _ -> None
let get_string_attr = function
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _}, _); _}] -> Some s
  | _ -> None

type field_attributes = {
  fa_field : string * bool * expression option;
  fa_key : string;
  fa_title : expression option;
  fa_description : expression option;
  fa_assoc : bool option;
  fa_enum : bool option;
  fa_exclude : expression option;
  fa_obj : bool option;
  fa_enc : expression option;
  fa_obj1 : string option;
  fa_merge : bool;
}

let field_attrs ~key ?(opt=false) ?(option="dft") l =
  let fa_field = match opt, option with
    | false, _ -> ("req", false, None)
    | _, "opt" -> ("opt", true, None)
    | _, "req" -> ("req", false, None)
    | _ -> ("dft", false, Some (enone ~loc:!Ast_helper.default_loc)) in
  List.fold_left (fun fa a -> match a.attr_name.txt with
      | "req" -> {fa with fa_field = ("req", false, None)}
      | "opt" -> {fa with fa_field = ("opt", true, None)}
      | "dft" -> {fa with fa_field = ("dft", false, get_expr_attr a.attr_payload)}
      | "key" -> begin match get_string_attr a.attr_payload with
          | None -> failwith "key expression must be a string constant"
          | Some fa_key -> {fa with fa_key} end
      | "title" -> {fa with fa_title = get_expr_attr a.attr_payload}
      | "description" -> {fa with fa_description = get_expr_attr a.attr_payload}
      | "assoc" -> {fa with fa_assoc = Some true}
      | "enum" -> {fa with fa_enum = Some true}
      | "exclude" -> {fa with fa_exclude = get_expr_attr a.attr_payload}
      | "object" -> {fa with fa_obj = Some true}
      | "encoding" -> {fa with fa_enc = get_expr_attr a.attr_payload}
      | "obj1" -> {fa with fa_obj1 = get_string_attr a.attr_payload}
      | "merge" -> {fa with fa_merge = true}
      | _ -> fa)
    {fa_field; fa_key=key; fa_title=None;
     fa_description=None; fa_assoc=None; fa_enum=None; fa_exclude=None;
     fa_obj=None; fa_enc=None; fa_obj1=None; fa_merge=false} l

type cs_attributes = {
  cs_kind : string option;
  cs_assoc : bool option;
  cs_enum : bool option;
  cs_key : string option;
  cs_obj : bool option;
  cs_enc : expression option;
  cs_title : expression option;
  cs_description : expression option;
  cs_ignore : bool;
  cs_rm_prefix : bool;
  cs_obj1 : string option;
}

let constructor_attrs l =
  List.fold_left (fun cs a -> match a.attr_name.txt with
      | "kind" -> {cs with cs_kind = get_string_attr a.attr_payload}
      | "assoc" -> {cs with cs_assoc = Some true}
      | "enum" -> {cs with cs_enum = Some true}
      | "key" -> {cs with cs_key = get_string_attr a.attr_payload}
      | "object" -> {cs with cs_obj = Some true}
      | "encoding" -> {cs with cs_enc = get_expr_attr a.attr_payload}
      | "title" -> {cs with cs_title = get_expr_attr a.attr_payload}
      | "description" -> {cs with cs_description = get_expr_attr a.attr_payload}
      | "ignore" -> {cs with cs_ignore = true}
      | "remove_prefix" -> {cs with cs_rm_prefix = true}
      | "obj1" -> {cs with cs_obj1 = get_string_attr a.attr_payload}
      | _ -> cs)
    {cs_kind=None; cs_assoc=None; cs_enum=None; cs_key=None; cs_obj=None;
     cs_enc=None; cs_title=None; cs_description=None; cs_ignore=false;
     cs_rm_prefix=false; cs_obj1=None} l

type core_attributes = {
  co_assoc : bool option;
  co_enum : bool option;
  co_exclude : expression option;
  co_obj : bool option;
  co_enc : expression option;
  co_obj1 : string option;
  co_merge : bool;
}

let core_attrs ?assoc ?enum ?obj ?enc ?obj1 l =
  List.fold_left (fun co a -> match a.attr_name.txt with
      | "assoc" -> {co with co_assoc = Some true}
      | "enum" -> {co with co_enum = Some true}
      | "exclude" -> {co with co_exclude = get_expr_attr a.attr_payload}
      | "object" -> {co with co_obj = Some true}
      | "encoding" -> {co with co_enc = get_expr_attr a.attr_payload}
      | "obj1" -> {co with co_obj1 = get_string_attr a.attr_payload}
      | "merge" -> {co with co_merge = true}
      | _ -> co)
    {co_assoc=assoc; co_enum=enum; co_exclude=None; co_obj=obj; co_enc=enc;
    co_obj1=obj1; co_merge=false} l

let new_var = let i = ref (-1) in fun () -> incr i; "v" ^ string_of_int !i

let str_of_structure e = Pprintast.string_of_structure e
let str_of_signature e =
  Pprintast.signature Format.str_formatter e;
  Format.flush_str_formatter ()

let rec encaps_tuple ~loc var tuple = function
  | [] -> assert false
  | [h] -> var ~loc h
  | h :: t -> tuple ~loc [var ~loc h; encaps_tuple ~loc var tuple t]

let rec encaps_merge ~loc ?(f="merge_objs") = function
  | [] -> assert false
  | [h, merge] -> if merge then h else enc_apply ~loc "obj1" [h]
  | [f1, m1; f2, m2] when not m1 && not m2 -> enc_apply ~loc "obj2" [f1; f2]
  | (h, merge) :: t -> enc_apply ~loc f [
      if merge then h else enc_apply ~loc "obj1" [h];
      encaps_merge ~loc ~f t ]

let obj_expr ~loc ?(kind="obj") l =
  let v = List.mapi (fun i _ -> "x" ^ string_of_int i) l in
  let no_merge = List.for_all (fun (_, b) -> not b) l in
  let n = List.length l in
  if n < 11 && no_merge then
    eapply ~loc (evar ~loc (enc_mod (kind ^ string_of_int n))) (List.map fst l)
  else
    let f = "merge_" ^ kind ^ "s" in
    enc_apply ~loc "conv" [
      pexp_fun ~loc (ppat_tuple ~loc (List.map (pvar ~loc) v)) (encaps_tuple ~loc evar pexp_tuple v);
      pexp_fun ~loc (encaps_tuple ~loc pvar ppat_tuple v) (pexp_tuple ~loc (List.map (evar ~loc) v));
      encaps_merge ~loc ~f l ]
