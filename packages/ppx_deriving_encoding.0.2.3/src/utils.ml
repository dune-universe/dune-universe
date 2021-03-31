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

let fake = match Sys.getenv_opt "PPX_ENCODING_FAKE" with
  | Some "1" | Some "true" | Some "yes" -> true
  | _ -> false

let debug ?(v=1) ?(force=false) fmt =
  if force || verbose >= v then Format.ksprintf (fun s -> Format.eprintf "%s@." s) fmt
  else Printf.ifprintf () fmt

let json_encoding_dot = ref "Json_encoding."
let json_encoding_tmp = ref !json_encoding_dot

let wrap = function
  | None -> json_encoding_tmp := !json_encoding_dot
  | Some "" -> json_encoding_tmp := !json_encoding_dot; json_encoding_dot := ""
  | Some s -> json_encoding_tmp := !json_encoding_dot; json_encoding_dot := String.capitalize_ascii s ^ "."

let unwrap () =
  json_encoding_dot := !json_encoding_tmp

let () = wrap (Sys.getenv_opt "PPX_ENCODING_MODULE")

let raise_error ~loc s = Location.raise_errorf ~loc s

let enc_names : (string, string) Hashtbl.t = Hashtbl.create 256

let enc_name ?(search=true) type_name =
  let aux name = if name = "t" then "enc" else name ^ "_enc" in
  if not search then aux type_name
  else match Hashtbl.find_opt enc_names type_name with
    | None -> aux type_name
    | Some name -> name

let add_enc_name type_name enc_name = Hashtbl.add enc_names type_name enc_name

let enc_mod s = !json_encoding_dot ^ s
let enc_var ~loc s = evar ~loc (enc_mod s)
let enc_apply ~loc s l = eapply ~loc (enc_var ~loc s) l

let pexp_fun ~loc p e = pexp_fun ~loc Nolabel None p e
let llid ~loc s = {txt=Longident.parse s; loc}
let esome ~loc e =
  pexp_construct ~loc (llid ~loc "Some") (Some (e ~loc))
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
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant cst; _}, _); _}] ->
    Compat.string_literal cst
  | _ -> None

let rm_prefix_of_expr e = match e.pexp_desc with
  | Pexp_construct ({txt=Lident "true"; _}, _) -> Some (`bool true)
  | Pexp_construct ({txt=Lident "false"; _}, _) -> Some (`bool false)
  | Pexp_constant (Pconst_integer (s, None)) -> Some (`length (int_of_string s))
  | Pexp_constant cst ->
    begin match Compat.string_literal cst with
      | Some s -> Some (`prefix s)
      | None -> None
    end
  | _ -> None

let get_rm_prefix_attr pl =
  match get_expr_attr pl with
  | None -> None
  | Some e -> rm_prefix_of_expr e

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
  fa_construct_default : bool;
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
      | "ddft" -> {fa with fa_field = ("dft", false, get_expr_attr a.attr_payload);
                           fa_construct_default = true}
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
    {fa_field; fa_key=key; fa_title=None; fa_description=None; fa_assoc=None;
     fa_enum=None; fa_exclude=None; fa_obj=None; fa_enc=None; fa_obj1=None;
     fa_merge=false; fa_construct_default=false} l

type cs_attributes = {
  cs_kind : string option;
  cs_kind_label : string option;
  cs_assoc : bool option;
  cs_enum : bool option;
  cs_key : string option;
  cs_obj : bool option;
  cs_enc : expression option;
  cs_title : expression option;
  cs_description : expression option;
  cs_ignore : bool;
  cs_rm_prefix : [`bool of bool | `prefix of string | `length of int];
  cs_obj1 : string option;
  cs_empty : bool option;
}

let constructor_attrs l =
  List.fold_left (fun cs a -> match a.attr_name.txt with
      | "kind" ->
        let cs_kind = match get_string_attr a.attr_payload with
          | None -> Some "" | k -> k in
        {cs with cs_kind}
      | "kind_label" -> {cs with cs_kind_label = get_string_attr a.attr_payload}
      | "assoc" -> {cs with cs_assoc = Some true}
      | "enum" -> {cs with cs_enum = Some true}
      | "key" -> {cs with cs_key = get_string_attr a.attr_payload}
      | "object" -> {cs with cs_obj = Some true}
      | "encoding" -> {cs with cs_enc = get_expr_attr a.attr_payload}
      | "title" -> {cs with cs_title = get_expr_attr a.attr_payload}
      | "description" -> {cs with cs_description = get_expr_attr a.attr_payload}
      | "ignore" -> {cs with cs_ignore = true}
      | "remove_prefix" ->
        let cs_rm_prefix = match get_rm_prefix_attr a.attr_payload with
          | None -> `bool true
          | Some x -> x in
        {cs with cs_rm_prefix}
      | "obj1" -> {cs with cs_obj1 = get_string_attr a.attr_payload}
      | "empty" -> {cs with cs_empty = Some true}
      | _ -> cs)
    {cs_kind=None; cs_assoc=None; cs_enum=None; cs_key=None; cs_obj=None;
     cs_enc=None; cs_title=None; cs_description=None; cs_ignore=false;
     cs_rm_prefix=`bool false; cs_obj1=None; cs_kind_label=None; cs_empty=None} l

type core_attributes = {
  co_assoc : bool option;
  co_enum : bool option;
  co_exclude : expression option;
  co_obj : bool option;
  co_enc : expression option;
  co_obj1 : string option;
  co_merge : bool;
  co_rm_prefix : [`bool of bool | `prefix of string | `length of int] option;
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
      | "remove_prefix" -> {co with co_rm_prefix = get_rm_prefix_attr a.attr_payload}
      | _ -> co)
    {co_assoc=assoc; co_enum=enum; co_exclude=None; co_obj=obj; co_enc=enc;
    co_obj1=obj1; co_merge=false; co_rm_prefix=None} l

let new_var = let i = ref (-1) in fun () -> incr i; "v" ^ string_of_int !i

let str_of_structure e = Pprintast.string_of_structure e
let str_of_signature e =
  Pprintast.signature Format.str_formatter e;
  Format.flush_str_formatter ()

let str_of_core e =
  Pprintast.core_type Format.str_formatter e;
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

let remove_prefix s n = String.sub s n (String.length s - n)

let same_prefix l =
  let common_prefix s1 s2 =
    let n1 = String.length s1 in
    let n2 = String.length s2 in
    let rec aux i =
      if i < n1 && i < n2 && s1.[i] = s2.[i] then aux (i+1)
      else i, String.sub s1 0 i in
    aux 0 in
  let rec aux n pr = function
    | [] -> n, pr
    | h :: t ->
      let n, pr = common_prefix h pr in
      aux n pr t in
  match l with
  | [] | [ _ ] -> 0
  | h :: t -> fst (aux (String.length h) h t)

module type S = sig
  type 'a encoding
  type 'a field
  type 'a case

  val unit : unit encoding
  val empty : unit encoding
  val int : int encoding
  val int32 : int32 encoding
  val int53 : int64 encoding
  val bool : bool encoding
  val string : string encoding
  val string_enum : (string * 'a) list -> 'a encoding
  val constant : string -> unit encoding
  val bytes : bytes encoding
  val float : float encoding
  val option : 'a encoding -> 'a option encoding

  val req : ?title:string -> ?description:string -> string -> 't encoding -> 't field
  val opt : ?title:string -> ?description:string -> string -> 't encoding -> 't option field
  val dft : ?title:string -> ?description:string -> string -> 't encoding -> 't -> 't field

  val obj1 : 'f1 field -> 'f1 encoding
  val obj2 : 'f1 field -> 'f2 field -> ('f1 * 'f2) encoding
  val obj3 : 'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding
  val obj4 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> ('f1 * 'f2 * 'f3 * 'f4) encoding
  val obj5 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field
    -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding
  val obj6 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding
  val obj7 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> 'f7 field -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding
  val obj8 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> 'f7 field -> 'f8 field -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding
  val obj9 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> 'f7 field -> 'f8 field -> 'f9 field
    -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding
  val obj10 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> 'f7 field -> 'f8 field -> 'f9 field -> 'f10 field
    -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

  val merge_objs : 'a encoding -> 'b encoding -> ('a * 'b) encoding

  val tup1 : 'f1 field -> 'f1 encoding
  val tup2 : 'f1 field -> 'f2 field -> ('f1 * 'f2) encoding
  val tup3 : 'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding
  val tup4 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> ('f1 * 'f2 * 'f3 * 'f4) encoding
  val tup5 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field
    -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding
  val tup6 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding
  val tup7 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> 'f7 field -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding
  val tup8 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> 'f7 field -> 'f8 field -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding
  val tup9 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> 'f7 field -> 'f8 field -> 'f9 field
    -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding
  val tup10 : 'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field -> 'f6 field
    -> 'f7 field -> 'f8 field -> 'f9 field -> 'f10 field
    -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

  val merge_tups : 'a encoding -> 'b encoding -> ('a * 'b) encoding

  val array : 'a encoding -> 'a array encoding
  val list : 'a encoding -> 'a list encoding
  val assoc : 'a encoding -> (string * 'a) list encoding
  val case : ?title:string -> ?description:string -> ('t encoding -> ('a -> 't option) -> ('t -> 'a)) list -> 'a case
  val union : 'a case list -> 'a encoding
  val conv : ('a -> 'b) -> ('b -> 'a) -> 'b encoding -> 'a encoding
  val mu : string -> ?title:string -> ?description:string -> ('a encoding -> 'a encoding) -> 'a encoding
  val any_ezjson_value : 'a encoding
  val def : string -> ?title:string -> ?description:string -> 'a encoding -> 'a encoding
end
