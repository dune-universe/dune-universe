open Base
open Ppxlib
open Ast_builder.Default

let raise_unsupported ~loc s =
  Location.raise_errorf ~loc
    "Unsupported use of %s (you can only use it on records)." s

type simple_processor =
  Location.t
  -> field_name:string
  -> expression

type recursive_processor =
  Location.t
  -> field_name:string
  -> type_name:string
  -> path:Longident.t option
  -> expression

module type Complete = sig
  val conversion_name : string
  val function_name : string option -> string
  val merge_recursive :
    Location.t -> field_name:string -> tp:core_type -> expression -> expression
  val unsupported_type_error_msg : name:string -> string
  val unit      : simple_processor
  val bool      : simple_processor
  val string    : simple_processor
  val char      : simple_processor
  val int       : simple_processor
  val float     : simple_processor
  val int32     : simple_processor
  val int64     : simple_processor
  val nativeint : simple_processor
  val big_int   : simple_processor
  val nat       : simple_processor
  val num       : simple_processor
  val ratio     : simple_processor
  val list      : simple_processor
  val array     : simple_processor
  val option    : simple_processor
  val ref       : simple_processor
  val lazy_t    : simple_processor
  val recursive : recursive_processor
end

module type Complete_list = sig
  include Complete
  val prepend : Location.t -> expression -> expression
end

module type Simple = sig

  val conversion_name : string
  val function_name : string option -> string
  val merge_recursive :
    Location.t -> field_name:string -> tp:core_type -> expression -> expression
  val unsupported_type_error_msg : name:string -> string
  val atoms : simple_processor
  val recursive : recursive_processor
end

module type Matcher = sig
  val conversion :
    Location.t
    -> field_name:string
    -> id:Longident.t Located.t
    -> expression

  val conversion_of_type :
    Location.t
    -> field_name:string
    -> field_ty:core_type
    -> expression
end

let conversion_of_type
  ~conversion
  ~conversion_name
  ~merge_recursive
  ~function_name
  loc ~field_name ~field_ty =
  let rec aux loc field_ty =
    match field_ty.ptyp_desc with
    | Ptyp_constr (id, args) ->
      let   id_expr = conversion loc ~field_name ~id in
      let args_expr = List.map args ~f:(aux loc) in
      merge_recursive loc ~field_name ~tp:field_ty (eapply ~loc id_expr args_expr)
    | Ptyp_var param ->
      evar ~loc (function_name (Some param))
    | _ ->
      Location.raise_errorf ~loc "%s: unsupported type construct"
        conversion_name
  in
  aux loc field_ty

module Of_simple (S : Simple) = struct
  let conversion loc ~field_name ~(id:Longident.t Located.t) =
    match id.txt with
    | Lident "unit"
    | Lident "bool"
    | Lident "string"
    | Lident "char"
    | Lident "int"
    | Lident "float"
    | Lident "int32"
    | Lident "int64"
    | Lident "nativeint"
    | Ldot (Lident "Big_int", "big_int")
    | Ldot (Lident "Nat", "nat")
    | Ldot (Lident "Num", "num")
    | Ldot (Lident "Ratio", "ratio") -> S.atoms loc ~field_name
    | Lident "ref"
    | Ldot (Lident "Lazy", "t")
    | Lident "lazy_t"
    | Lident "sexp_option"
    | Lident "option"
    | Lident "list"
    | Lident "array"
    | Ldot (Lident "Hashtbl", "t")
    | Lident "bigstring"
    | Lident "vec"
    | Lident "float32_vec"
    | Lident "float64_vec"
    | Lident "mat"
    | Lident "float32_mat"
    | Lident "float64_mat"
    | Lident "exn" ->
      let name = Longident.last_exn id.txt in
      Location.raise_errorf ~loc "%s"
        (S.unsupported_type_error_msg ~name)
    | Ldot (path, type_name) -> S.recursive loc ~field_name ~type_name ~path:(Some path)
    | Lident type_name -> S.recursive loc ~field_name ~type_name ~path:None
    | Lapply _ -> assert false

  let conversion_of_type =
    conversion_of_type ~conversion ~conversion_name:S.conversion_name
      ~function_name:S.function_name ~merge_recursive:S.merge_recursive
end

module Of_complete (S : Complete) = struct

  let conversion loc ~field_name ~(id:Longident.t Located.t) =
    match id.txt with
    | Lident "unit"                      -> S.unit loc ~field_name
    | Lident "bool"                      -> S.bool loc ~field_name
    | Lident "string"                    -> S.string loc ~field_name
    | Lident "char"                      -> S.char loc ~field_name
    | Lident "int"                       -> S.int loc ~field_name
    | Lident "decimal"                   -> S.float loc ~field_name
    | Lident "float"                     -> S.float loc ~field_name
    | Lident "int32"                     -> S.int32 loc ~field_name
    | Lident "int64"                     -> S.int64 loc ~field_name
    | Lident "nativeint"                 -> S.nativeint loc ~field_name
    | Ldot (Lident "Big_int", "big_int") -> S.big_int loc ~field_name
    | Ldot (Lident "Nat", "nat")         -> S.nat loc ~field_name
    | Ldot (Lident "Num", "num")         -> S.num loc ~field_name
    | Ldot (Lident "Ratio", "ratio")     -> S.ratio loc ~field_name
    | Lident "list"                      -> S.list loc ~field_name
    | Lident "array"                     -> S.array loc ~field_name
    | Lident "sexp_option"
    | Lident "option"                    -> S.option loc ~field_name
    | Lident "ref"
    | Ldot (Lident "Lazy", "t")
    | Lident "lazy_t"
    | Ldot (Lident "Hashtbl", "t")
    | Lident "bigstring"
    | Lident "vec"
    | Lident "float32_vec"
    | Lident "float64_vec"
    | Lident "mat"
    | Lident "float32_mat"
    | Lident "float64_mat"
    | Lident "exn" ->
      let name = Longident.last_exn id.txt in
      Location.raise_errorf ~loc "%s" (S.unsupported_type_error_msg ~name)
    | Ldot (path, type_name) -> S.recursive loc ~field_name ~type_name ~path:(Some path)
    | Lident type_name -> S.recursive loc ~field_name ~type_name ~path:None
    | Lapply _ -> assert false  (* impossible *)

  let conversion_of_type =
    conversion_of_type ~conversion ~conversion_name:S.conversion_name
      ~function_name:S.function_name ~merge_recursive:S.merge_recursive
end

module Of_list (P : Complete_list) = struct
  include Of_complete (P)
  let conversion_of_type _loc ~field_name ~field_ty =
    P.prepend _loc (conversion_of_type _loc ~field_name ~field_ty)
end

let lambda loc ps e = eabstract ~loc ps e

module Gen_sig = struct
  (*let label_arg _loc name ty = Ast.TyLab (_loc, name, ty)

  let rec loop _loc this_type output_type = function
    | [] -> <:ctyp< $this_type$ -> $output_type$ >>
    | tp :: tps ->
        let tp = Gen.drop_variance_annotations tp in
        let row_of = loop _loc <:ctyp< $this_type$ $tp$ >> output_type tps in
        <:ctyp< ( $tp$ -> $output_type$) -> $row_of$ >>

  let row_of_t' ~record_name ~tps _loc =
    let t = loop _loc <:ctyp< $lid:record_name$ >> <:ctyp< unit >> tps in
    let is_first = label_arg _loc "is_first" <:ctyp< bool >> in
    let is_last = label_arg _loc "is_last" <:ctyp< bool >> in
    let writer = label_arg _loc "writer" <:ctyp< string -> unit >> in
    <:sig_item< value $lid: "write_row_of_" ^ record_name ^ "'"$ :
      $is_first$ -> $is_last$ -> $writer$ -> _ -> _ -> $t$ >>
  ;;

  let t_of_row' ~record_name _loc =
    let f = <:ctyp< unit -> $lid: record_name$ >> in
    let pair = <:ctyp< $f$ * (string list) >> in
    <:sig_item< value $lid: record_name ^ "_of_row'"$ : _ -> string list -> $pair$ >>
  ;;*)

  let fields_of_ty td ~extension_name ~nil ~record =
    let loc = td.ptype_loc in
    let unsupported () = raise_unsupported ~loc extension_name in
    let tps = List.map td.ptype_params ~f:fst in
    match td.ptype_kind with
    | Ptype_open | Ptype_variant _ -> unsupported ()
    | Ptype_record lds -> record ~tps ~record_name:td.ptype_name.txt loc lds
    | Ptype_abstract ->
      match td.ptype_manifest with
      | Some { ptyp_desc = Ptyp_variant _; _ } -> unsupported ()
      | _ -> nil ~tps ~record_name:td.ptype_name.txt loc

  let generate ~extension_name ~nil ~record ~loc ~path:_ (_rf, tds) =
    match tds with
    | [td] ->
      fields_of_ty td ~extension_name ~nil ~record
    | _ -> raise_unsupported ~loc extension_name
end

let arg_label_of_string s : Asttypes.arg_label =
  if String.is_empty s then
    Nolabel
  else if Char.equal s.[0] '?' then
    Optional (String.drop_prefix s 1)
  else
    Labelled s

module Gen_struct = struct

  (*let label_arg ?label _loc name =
    let l =
      match label with
      | None    -> name
      | Some n  -> n in
    Ast.PaLab (_loc, l, <:patt< $lid:name$ >> )
  ;;*)

  let field ld =
    let mutability =
      match ld.pld_mutable with
      | Mutable   -> `Mutable
      | Immutable -> `Immutable
    in
    (ld.pld_name.txt, mutability, ld.pld_type)
  ;;

  let fields lds = List.map lds ~f:field

  let value default opt =
    match opt with
    | None -> default
    | Some v -> v

  let make_body ?unique_f ?first_f ?last_f ~lds ~init loc middle_f =
    let unique_f = value middle_f unique_f in
    let first_f = value middle_f first_f in
    let last_f = value middle_f last_f in
    let add_one_field f acc (field_name, _kind, field_ty) =
      let f = f loc ~field_name ~field_ty in
      pexp_apply ~loc acc [(arg_label_of_string field_name, f)]
    in
    let fields = fields lds in
    match fields with
    | [] -> assert false
    | [h] -> add_one_field unique_f init h
    | first :: t ->
      match List.rev t with
      | [] -> assert false
      | last :: t ->
        let t = List.rev t in
        let init = add_one_field first_f init first in
        let init = List.fold_left t ~init ~f:(add_one_field middle_f) in
        add_one_field last_f init last

  let anonymous loc = [%pat?  _ ]

  let ident x = x

  let generate_using_fold ?(wrap_body=ident) ~pass_acc ~pass_anonymous
  ~conversion_of_type ~name ~lds loc =
    let acc = [%pat? acc ] in
    let init =
      if pass_acc then [%expr  Fields.fold ~init:acc ]
      else [%expr  Fields.fold ~init:[] ]
    in
    let body = make_body ~lds ~init loc conversion_of_type in
    let anonymous = anonymous loc in
    let func =
      let arguments =
        if pass_anonymous then
          [ anonymous;
            anonymous;
          ]
        else []
      in
      let arguments = if pass_acc then acc :: arguments else arguments in
      let body = wrap_body body in
      match arguments with
      | [] ->  body
      | arguments -> lambda loc arguments body
    in
    pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:name ~expr:func ]

  let fields_of_ty td ~extension_name ~record =
    let loc = td.ptype_loc in
    let unsupported () = raise_unsupported ~loc extension_name in
    let tps = List.map td.ptype_params ~f:fst in
    match td.ptype_kind with
    | Ptype_open | Ptype_variant _ -> unsupported ()
    | Ptype_record lds -> record ~tps ~record_name:td.ptype_name.txt loc lds
    | Ptype_abstract -> unsupported ()

  let generate ~extension_name ~record ~loc ~path:_ (_rf, tds) =
    match tds with
    | [td] ->
      fields_of_ty td ~extension_name ~record
    | _ -> raise_unsupported ~loc extension_name
end
