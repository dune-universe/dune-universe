module String_hashtbl = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash = Hashtbl.hash
end)

module String_map = Map.Make (struct
  type t = string

  let compare = compare
end)

let output_subst f channel template =
  let buffer = Buffer.create (String.length template) in
  Buffer.add_substitute buffer f template;
  Buffer.output_buffer channel buffer

let loc txt =
  { Location.txt; loc = !Ppxlib.Ast_helper.default_loc }

let make_ocaml_type_name s =
  let buffer = Buffer.create 17 in
  String.iter (fun c ->
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' ->
        Buffer.add_char buffer (Char.lowercase_ascii c)
    | _ -> ()) s;
  Buffer.contents buffer

type converter =
    out_channel -> src:string -> params:string array -> references:string array
      -> tgt:string -> unit

type common_type_info = {
    ocamltype : Ppxlib.core_type;
    c_of_ocaml : converter;
    ocaml_of_c : converter;
  }

type enum_info = {
    result : string option;
    constructors : (string * string * int) list;
  }

type struct_info = unit

type type_info =
  | Void
  | Regular
  | Bool
  | Not_bool
  | Enum of enum_info
  | Struct of struct_info

type argument =
  | Index of int
  | Name of string

type value =
  | Fixed of int * string
  | Argument of argument

type type_spec =
  | Int
  | Enum of string
  | Sized_array of { length : value }
  | Array_struct of { contents : string; length : string }
  | Sized_string of { length : argument }
  | Set_of of string
  | Type_info of common_type_info * type_info

type type_interface = {
    reinterpret_as : type_spec option;
    destructor : (string -> string) option;
    carry_reference : string option;
    null_is_none : bool;
    compare : string option;
    hash : string option;
  }

let empty_type_interface =
  { reinterpret_as = None; destructor = None; carry_reference = None;
    null_is_none = false;
    compare = None;
    hash = None; }

let union_option a b =
  match a, b with
  | None, other
  | other, None -> other
  | Some a, Some b -> Some b

let union_type_interfaces a b =
  { reinterpret_as = union_option a.reinterpret_as b.reinterpret_as;
    destructor = union_option a.destructor b.destructor;
    carry_reference = union_option a.carry_reference b.carry_reference;
    null_is_none = a.null_is_none || b.null_is_none;
    compare = union_option a.compare b.compare;
    hash = union_option a.hash b.hash; }

let reinterpret_as type_spec type_interface =
  { type_interface with reinterpret_as = union_option type_interface.reinterpret_as (Some type_spec) }

let destructor destructor type_interface =
  { type_interface with destructor = union_option type_interface.destructor (Some destructor) }

let carry_reference type_name type_interface =
  { type_interface with carry_reference = Some type_name }

let integer_enum enum = reinterpret_as (Enum enum)

let integer_boolean = integer_enum "bool"

let integer_zero_is_true = integer_enum "not bool"

let null_is_none type_interface =
  { type_interface with null_is_none = true }

let compare_value compare type_interface =
  { type_interface with compare = Some compare }

let hash_value hash type_interface =
  { type_interface with hash = Some hash }

type argument_interface =
  | Array of { length : value; contents : argument }
  | Sized_string of { length : value; contents : argument }
  | Output of { argument : argument; on_success : bool; on_error : bool }
  | Update of argument
  | Fixed_value of { argument : argument; value : string }
  | Type_interface of { argument : argument; interface : type_interface }
  | Closure of {
      pointer : argument;
      data_caller : argument;
      data_callee : argument;
      references : argument list;
    }

let output argument =
  Output { argument; on_success = true; on_error = true }

let output_on_success argument =
  Output { argument; on_success = true; on_error = false }

let output_on_error argument =
  Output { argument; on_success = false; on_error = true }

type function_interface = {
    hidden : bool;
    label_unique : bool;
    result : type_interface;
    rename : string -> string;
    arguments : argument_interface list;
  }

let empty_function_interface =
  { hidden = false;
    label_unique = true;
    result = empty_type_interface;
    rename = (fun x -> x);
    arguments = [] }

let hidden_function_interface = { empty_function_interface with hidden = true }

let dont_label_unique interface = { interface with label_unique = false }

let rename_function rename =
  { empty_function_interface with rename }

let union_function_interfaces a b =
  { hidden = a.hidden || b.hidden;
    label_unique = a.label_unique && b.label_unique;
    result = union_type_interfaces a.result b.result;
    rename = (fun name -> a.rename (b.rename name));
    arguments = List.rev_append a.arguments b.arguments }

let add_argument argument function_interface =
  { function_interface with
    arguments = argument :: function_interface.arguments }

let add_result type_interface function_interface =
  { function_interface with
    result = union_type_interfaces function_interface.result type_interface }

type constant_interface = {
    success : bool;
    preferred : bool;
  }

let empty_constant_interface = {
    success = false;
    preferred = false;
  }

let set_success interface = {
    interface with success = true
  }

let set_preferred interface = {
    interface with preferred = true
  }

let union_constant_interfaces a b = {
    success = a.success || b.success;
    preferred = a.preferred || b.preferred;
  }

type enum_interface = {
    constants : (Pcre.regexp * constant_interface) list;
    attributes : Ppxlib.attributes;
  }

let empty_enum_interface = { constants = []; attributes = []; }

let union_enum_interfaces a b =
  { constants = List.rev_append a.constants b.constants;
    attributes = a.attributes @ b.attributes; }

let add_constant constant_name constant_interface enum_interface =
  { enum_interface with constants =
    (constant_name, constant_interface) :: enum_interface.constants }

let add_attributes attributes enum_interface =
  { enum_interface with attributes = enum_interface.attributes @ attributes }

type field_interface =
  | Sized_string of { length : string; contents : string }

type accessor_interface = {
    field_name : string;
    accessor_name : string;
    stub_name : string;
  }

type struct_interface = {
    fields : field_interface list;
    accessors : accessor_interface list;
  }

let empty_struct_interface = { fields = []; accessors = [] }

let union_struct_interfaces a b =
  { fields = List.rev_append a.fields b.fields;
    accessors = List.rev_append a.accessors b.accessors }

let add_field field_interface struct_interface =
  { struct_interface with
    fields = field_interface :: struct_interface.fields }

let add_accessor field_name accessor_name stub_name struct_interface =
  { struct_interface with
    accessors = { field_name; accessor_name; stub_name } ::
      struct_interface.accessors }

type module_interface = {
    types : (Pcre.regexp * type_interface) list;
    functions : (Pcre.regexp * function_interface) list;
    enums : (Pcre.regexp * enum_interface) list;
    structs : (Pcre.regexp * struct_interface) list;
  }

let empty_module_interface = { types = []; functions = []; enums = []; structs = [] }

let get_interface empty union list name =
  let add_rule accu (rex, interface) =
    if Pcre.pmatch ~rex name then
      union interface accu
    else
      accu in
  List.fold_left add_rule empty list

let has_prefix prefix s =
  String.length s >= String.length prefix &&
  prefix = String.sub s 0 (String.length prefix)

let remove_prefix prefix s =
  if has_prefix prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else
    s

let get_type type_name module_interface =
  let type_name = remove_prefix "struct " type_name in
  get_interface empty_type_interface union_type_interfaces
    module_interface.types type_name

let get_function function_name module_interface =
  get_interface empty_function_interface union_function_interfaces
    module_interface.functions function_name

let get_enum enum_name module_interface =
  get_interface empty_enum_interface union_enum_interfaces
    module_interface.enums enum_name

let get_constant constant_name enum_interface =
  get_interface empty_constant_interface union_constant_interfaces
    enum_interface.constants constant_name

let get_struct struct_name module_interface =
  get_interface empty_struct_interface union_struct_interfaces
    module_interface.structs struct_name

let add_type type_name type_interface module_interface =
  { module_interface with types =
    (type_name, type_interface) :: module_interface.types }

let add_function function_name function_interface module_interface =
  { module_interface with functions =
    (function_name, function_interface) :: module_interface.functions }

let add_enum enum_name enum_interface module_interface =
  { module_interface with enums =
    (enum_name, enum_interface) :: module_interface.enums }

let add_struct struct_name struct_interface module_interface =
  { module_interface with structs =
    (struct_name, struct_interface) :: module_interface.structs }

let simple_converter name fmt ~src ~params ~references ~tgt =
  Printf.fprintf fmt "%s = %s(%s);" tgt name src

let name_of_c_of_ocaml converter =
  Printf.sprintf "%s_val" (String.capitalize_ascii converter)

let name_of_ocaml_of_c converter =
  Printf.sprintf "Val_%s" converter

let make_common_type_info ?type_interface ?converter ocaml_type_name =
  let converter =
    match converter with
    | None -> ocaml_type_name
    | Some converter -> converter in
  let c_of_ocaml = name_of_c_of_ocaml converter in
  let ocaml_of_c = name_of_ocaml_of_c converter in
  let c_of_ocaml, ocaml_of_c =
    match type_interface with
    | Some { carry_reference = Some _ } ->
        let c_of_ocaml fmt ~src ~params ~references ~tgt =
          Printf.fprintf fmt "%s = %s(Field(%s, 0));" tgt c_of_ocaml src in
        let ocaml_of_c fmt ~src ~params ~references ~tgt =
          Printf.fprintf fmt "\
  %s = caml_alloc_tuple(%d);
  Store_field(%s, 0, %s(%s));" tgt (1 + Array.length references) tgt ocaml_of_c src;
          references |> Array.iteri @@ fun i reference ->
     Printf.fprintf fmt "\n  Store_field(%s, %d, %s);" tgt (i + 1) reference in
        c_of_ocaml, ocaml_of_c
    | _ ->
        simple_converter c_of_ocaml, simple_converter ocaml_of_c in
  { ocamltype = Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident ocaml_type_name)) [];
    c_of_ocaml; ocaml_of_c; }

type translation_context = {
    module_interface : module_interface;
    chan_stubs : out_channel;
    type_table : (common_type_info Lazy.t * type_info) String_hashtbl.t;
    enum_table : (common_type_info Lazy.t * enum_info) String_hashtbl.t;
    struct_table : (common_type_info Lazy.t * struct_info) String_hashtbl.t;
    used_type_table : unit String_hashtbl.t;
    mutable sig_accu : Ppxlib.signature_item list;
    mutable struct_accu : Ppxlib.structure_item list;
  }

let create_translation_context module_interface chan_stubs =
  let used_type_table = String_hashtbl.of_seq (List.to_seq ["int", ()]) in
  {
    module_interface;
    chan_stubs;
    type_table = String_hashtbl.create 17;
    enum_table = String_hashtbl.create 17;
    struct_table = String_hashtbl.create 17;
    used_type_table;
    sig_accu = [];
    struct_accu = [];
  }

let make_name_unique used_names name =
  let name =
    if String_hashtbl.mem used_names name then
      let rec add_index index =
        let new_name = Printf.sprintf "%s%d" name index in
        if String_hashtbl.mem used_names new_name then
          add_index (succ index)
        else
          new_name in
      add_index 2
    else
      name in
  String_hashtbl.add used_names name ();
  name

exception Unknown_type

let ocaml_string = Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "string")) []

type elaborated_type =
  | Enum of string
  | Struct of string

let check_prefix prefix string =
  let prefix_length = String.length prefix in
  let string_length = String.length string in
  if string_length >= prefix_length &&
      String.sub string 0 prefix_length = prefix then
    Some (String.sub string prefix_length (string_length - prefix_length))
  else
    None

let get_elaborated_type type_spelling =
  match check_prefix "enum " type_spelling with
  | Some type_name -> Some (Enum type_name)
  | None ->
      match check_prefix "struct " type_spelling with
      | Some type_name -> Some (Struct type_name)
      | None -> None

let int_info = make_common_type_info "int"

let bool_info = make_common_type_info "bool"

let not_bool_info = make_common_type_info "bool" ~converter:"not_bool"

let ocaml_array ty =
  Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "array")) [ty]

let ocaml_option ty =
  Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "option")) [ty]

let string_type_info =
  { ocamltype = ocaml_string;
    c_of_ocaml = simple_converter "String_val";
    ocaml_of_c = simple_converter "caml_copy_string"; }, Regular

let int64_type_info =
  { ocamltype = Ppxlib.Ast_helper.Typ.constr (loc (Longident.Ldot (Longident.Lident "Int64", "t"))) [];
    c_of_ocaml = simple_converter "Int64_val";
    ocaml_of_c = simple_converter "copy_int64"; }, Regular

let defined_set_of = String_hashtbl.create 17

let add_sig_type context ty =
  context.sig_accu <- Ppxlib.Ast_helper.Sig.type_ Recursive ty :: context.sig_accu;
  context.struct_accu <- Ppxlib.Ast_helper.Str.type_ Recursive ty :: context.struct_accu

let add_primitive context v =
  context.sig_accu <- Ppxlib.Ast_helper.Sig.value v :: context.sig_accu;
  context.struct_accu <- Ppxlib.Ast_helper.Str.primitive v :: context.struct_accu

let escape_doc doc =
  Pcre.replace ~pat:"[][@{}]" ~templ:"\\$&" doc

let make_doc_attributes cur =
  match Clang.cursor_get_brief_comment_text cur with
  | None -> []
  | Some doc ->
      [Ppxlib.Ast_helper.Attr.mk (loc "ocaml.doc")
         (Ppxlib.PStr [Ppxlib.Ast_helper.Str.eval
           (Ppxlib.Ast_helper.Exp.constant (Ppxlib.Ast_helper.Const.string (escape_doc doc)))])]

let rec find_type_info ?(declare_abstract = true) ?parameters context type_interface ty =
  let find_enum_info type_name =
    let enum_info =
      try
        String_hashtbl.find context.enum_table type_name
      with Not_found ->
        failwith ("Unknown enum " ^ type_name) in
    let common_info, enum_info = enum_info in
    Lazy.force common_info, (Enum enum_info : type_info) in
  let type_name = Clang.get_type_spelling ty in
  let type_interface = union_type_interfaces
      (get_type type_name context.module_interface) type_interface in
  let default_type type_name =
    match type_interface.reinterpret_as with
    | Some Int ->
        { ocamltype = int_info.ocamltype;
          c_of_ocaml = (fun fmt ~src ~params ~references ~tgt ->
            Printf.fprintf fmt "%s = (%s) Int_val(%s);" tgt type_name src);
          ocaml_of_c = (fun fmt ~src ~params ~references ~tgt ->
            Printf.fprintf fmt "%s = Val_int((int) %s);" tgt src); }, Regular
    | Some (Type_info (common_type_info, type_info)) -> common_type_info, type_info
    | None ->
        begin
    match String_hashtbl.find_opt context.type_table type_name with
    | Some (common_info, type_info) ->
        Lazy.force common_info, type_info
    | None ->
        if not declare_abstract then
          raise Unknown_type;
        let ocaml_type_name = make_ocaml_type_name type_name in
        let ocaml_type_name =
          make_name_unique context.used_type_table ocaml_type_name in
        let ocamltype =
          Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident ocaml_type_name)) [] in
        let common_info, type_info =
          { ocamltype; c_of_ocaml = simple_converter "";
            ocaml_of_c = simple_converter "" },
          Regular in
        String_hashtbl.add context.type_table type_name (lazy common_info, type_info);
        add_sig_type context [Ppxlib.Ast_helper.Type.mk (loc ocaml_type_name)];
        common_info, type_info
        end
    | _ ->
        prerr_endline (Clang.get_type_spelling ty);
        assert false in
  let common_info, type_info =
  match Clang.get_type_kind ty with
  | Void ->
      { ocamltype = Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "unit")) [];
        c_of_ocaml = (fun _ -> assert false);
        ocaml_of_c = (fun _ -> assert false); }, Void
  | UInt
  | Int
  | Long
  | ULong
  | LongLong
  | ULongLong ->
      begin
        match type_interface.reinterpret_as with
        | Some (Enum "bool") -> bool_info, Bool
        | Some (Enum "not bool") -> not_bool_info, Not_bool
        | Some (Enum enum) -> find_enum_info enum
        | Some (Set_of enum) ->
            let ocaml_type_name = String.lowercase_ascii enum in
            let mod_name = String.capitalize_ascii ocaml_type_name in
            let type_info =  
  { ocamltype = Ppxlib.Ast_helper.Typ.constr (loc (Longident.Ldot (Longident.Lident mod_name, "t"))) [];
    c_of_ocaml =
      simple_converter (name_of_c_of_ocaml "int");
    ocaml_of_c = simple_converter (name_of_ocaml_of_c "int"); } in
            let () =
              if not (String_hashtbl.mem defined_set_of ocaml_type_name) then
                begin
                  String_hashtbl.add defined_set_of ocaml_type_name ();
                  let _, enum_info =
                    try String_hashtbl.find context.enum_table enum
                    with Not_found ->
                    try
                      match String_hashtbl.find context.type_table enum with
                      | x, Enum enum_info -> x, enum_info
                      | _ -> assert false
                    with Not_found -> failwith ("not found " ^ enum) in
                  let t = Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "t")) [] in
                  let bind_value name value =
                    (Ppxlib.Ast_helper.Str.value Nonrecursive [Ppxlib.Ast_helper.Vb.mk (Ppxlib.Ast_helper.Pat.var (loc (Stubgen_common.uncamelcase name))) (Ppxlib.Ast_helper.Exp.constant (Ppxlib.Ast_helper.Const.int value))],
                     Ppxlib.Ast_helper.Sig.value (Ppxlib.Ast_helper.Val.mk (loc (Stubgen_common.uncamelcase name)) t)) in
                  let has_zero = enum_info.constructors |> List.exists @@ fun (_, _, value) -> value = 0 in
                  let zero_value =
                    if has_zero then []
                    else [bind_value "zero" 0] in
                  let items =
                    (Ppxlib.Ast_helper.Str.type_ Recursive [Ppxlib.Ast_helper.Type.mk (loc "t") ~manifest:int_info.ocamltype],
                     Ppxlib.Ast_helper.Sig.type_ Recursive [Ppxlib.Ast_helper.Type.mk (loc "t")]) ::
                    (Ppxlib.Ast_helper.Str.primitive (Ppxlib.Ast_helper.Val.mk (loc "+") (Ppxlib.Ast_helper.Typ.arrow Nolabel t (Ppxlib.Ast_helper.Typ.arrow Nolabel t t)) ~prim:["%orint"]),
                     Ppxlib.Ast_helper.Sig.value (Ppxlib.Ast_helper.Val.mk (loc "+") (Ppxlib.Ast_helper.Typ.arrow Nolabel t (Ppxlib.Ast_helper.Typ.arrow Nolabel t t)) ~prim:["%orint"])) ::
                    (Ppxlib.Ast_helper.Str.value Nonrecursive [Ppxlib.Ast_helper.Vb.mk (Ppxlib.Ast_helper.Pat.var (loc "-")) (Ppxlib.Ast_helper.Exp.fun_ Nolabel None (Ppxlib.Ast_helper.Pat.var (loc "x")) (Ppxlib.Ast_helper.Exp.fun_ Nolabel None (Ppxlib.Ast_helper.Pat.var (loc "y")) (Ppxlib.Ast_helper.Exp.apply (Ppxlib.Ast_helper.Exp.ident (loc (Longident.Lident "land"))) [Nolabel, Ppxlib.Ast_helper.Exp.ident (loc (Longident.Lident "x")); Nolabel, Ppxlib.Ast_helper.Exp.apply (Ppxlib.Ast_helper.Exp.ident (loc (Longident.Lident "lnot"))) [Nolabel, Ppxlib.Ast_helper.Exp.ident (loc (Longident.Lident "y"))]])))],
                     Ppxlib.Ast_helper.Sig.value (Ppxlib.Ast_helper.Val.mk (loc "-") (Ppxlib.Ast_helper.Typ.arrow Nolabel t (Ppxlib.Ast_helper.Typ.arrow Nolabel t t)))) ::
                    (Ppxlib.Ast_helper.Str.primitive (Ppxlib.Ast_helper.Val.mk (loc "&") (Ppxlib.Ast_helper.Typ.arrow Nolabel t (Ppxlib.Ast_helper.Typ.arrow Nolabel t t)) ~prim:["%andint"]),
                     Ppxlib.Ast_helper.Sig.value (Ppxlib.Ast_helper.Val.mk (loc "&") (Ppxlib.Ast_helper.Typ.arrow Nolabel t (Ppxlib.Ast_helper.Typ.arrow Nolabel t t)) ~prim:["%andint"])) ::
                    (Ppxlib.Ast_helper.Str.primitive (Ppxlib.Ast_helper.Val.mk (loc "*") (Ppxlib.Ast_helper.Typ.arrow Nolabel t (Ppxlib.Ast_helper.Typ.arrow Nolabel t t)) ~prim:["%xorint"]),
                     Ppxlib.Ast_helper.Sig.value (Ppxlib.Ast_helper.Val.mk (loc "*") (Ppxlib.Ast_helper.Typ.arrow Nolabel t (Ppxlib.Ast_helper.Typ.arrow Nolabel t t)) ~prim:["%xorint"])) ::
                    (Ppxlib.Ast_helper.Str.value Nonrecursive [Ppxlib.Ast_helper.Vb.mk (Ppxlib.Ast_helper.Pat.var (loc "subset")) (Ppxlib.Ast_helper.Exp.fun_ Nolabel None (Ppxlib.Ast_helper.Pat.var (loc "x")) (Ppxlib.Ast_helper.Exp.fun_ Nolabel None (Ppxlib.Ast_helper.Pat.var (loc "y")) (Ppxlib.Ast_helper.Exp.apply (Ppxlib.Ast_helper.Exp.ident (loc (Longident.Lident "="))) [Nolabel, Ppxlib.Ast_helper.Exp.apply (Ppxlib.Ast_helper.Exp.ident (loc (Longident.Lident "-"))) [Nolabel, Ppxlib.Ast_helper.Exp.ident (loc (Longident.Lident "y")); Nolabel, Ppxlib.Ast_helper.Exp.ident (loc (Longident.Lident "x"))]; Nolabel, Ppxlib.Ast_helper.Exp.constant (Ppxlib.Ast_helper.Const.integer "0")])))],
                     Ppxlib.Ast_helper.Sig.value (Ppxlib.Ast_helper.Val.mk (loc "subset") (Ppxlib.Ast_helper.Typ.arrow Nolabel t (Ppxlib.Ast_helper.Typ.arrow Nolabel t bool_info.ocamltype)))) ::
                    zero_value @
                    (enum_info.constructors |> List.map @@ fun (_, ocaml_name, value) ->
                      bind_value ocaml_name value) in
                  let m, s = List.split items  in
                  context.sig_accu <- Ppxlib.Ast_helper.Sig.module_ (Metapp.Md.mk (loc (Some mod_name)) (Ppxlib.Ast_helper.Mty.signature s)) :: context.sig_accu;
                  context.struct_accu <- Ppxlib.Ast_helper.Str.module_ (Metapp.Mb.mk (loc (Some mod_name)) (Ppxlib.Ast_helper.Mod.structure m)) :: context.struct_accu
                end in
            type_info, Regular
        | None -> int_info, Regular
        | Some _ -> assert false
      end
  | Bool ->
      bool_info, Bool
  | Float | Double ->
      { ocamltype = Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "float")) [];
        c_of_ocaml = simple_converter "Double_val";
        ocaml_of_c = simple_converter "caml_copy_double"; }, Regular
  | Pointer when Clang.get_type_kind (Clang.get_pointee_type ty) = Char_S ->
      begin
        match type_interface.reinterpret_as with
        | Some (Sized_string { length }) ->
            begin
              match parameters with
              | None -> assert false
              | Some parameters ->
                  parameters := [| length |]
            end;
            let ocamltype, template_c_of_ocaml, template_ocaml_of_c =
                ocaml_string, "  \
  $length = caml_string_length($src);
  $tgt = String_val($src);
", "  \
  $tgt = caml_alloc_initialized_string($length, $src);
  $destructor
" in
          let subst ~src ~tgt ~length var =
            match var with
            | "src" -> src
            | "tgt" -> tgt
            | "length" -> length
            | "destructor" ->
                begin
                  match type_interface.destructor with
                  | None -> ""
                  | Some destructor -> destructor src
                end
            | _ -> assert false in
          let converter template channel ~src ~params ~references ~tgt =
            let length = params.(0) in
            output_subst (subst ~src ~tgt ~length) channel template in
          { ocamltype;
            c_of_ocaml = converter template_c_of_ocaml;
            ocaml_of_c = converter template_ocaml_of_c; }, Regular
        | None ->
            string_type_info
        | _ -> assert false
      end
  | Pointer ->
      begin
        match type_interface.reinterpret_as with
        | Some (Sized_array { length }) ->
            let length, length_ty =
              match length with
              | Fixed (length, length_ty) -> length, length_ty
              | Argument _ -> assert false in
            let contents_type_info, _ = find_type_info ~declare_abstract context empty_type_interface (Clang.get_pointee_type ty) in
            if length = 1 then
            { ocamltype = contents_type_info.ocamltype;
              c_of_ocaml = simple_converter "";
              ocaml_of_c = (fun channel ~src ~params ~references ~tgt ->
                Printf.fprintf channel "
%t;
"
(fun channel -> contents_type_info.ocaml_of_c channel ~src:(Printf.sprintf "%s[0]" src) ~params:[| |] ~references ~tgt)); }, Regular
            else
            { ocamltype = ocaml_array contents_type_info.ocamltype;
              c_of_ocaml = simple_converter "";
              ocaml_of_c = (fun channel ~src ~params ~references ~tgt ->
                Printf.fprintf channel "
%t;
"
(fun channel -> contents_type_info.ocaml_of_c channel ~src:(Printf.sprintf "%s[0]" src) ~params:[| |] ~references ~tgt)); }, Regular
        | Some (Array_struct { length; contents }) ->
            let pointee = Clang.get_canonical_type (Clang.get_pointee_type ty) in
            let field_types = pointee |> Clang.list_of_type_fields |> List.map @@ fun cur ->
              Clang.get_cursor_spelling cur, Clang.get_cursor_type cur in
            let find_field name =
              try List.assoc name field_types
              with Not_found -> failwith (Printf.sprintf "Unknown field: %s" name) in
            let length_ty = find_field length in
            let contents_ty = find_field contents in
            let contents_type_info, _ = find_type_info ~declare_abstract context empty_type_interface (Clang.get_pointee_type contents_ty) in
            { ocamltype = ocaml_array contents_type_info.ocamltype;
              c_of_ocaml = simple_converter "";
              ocaml_of_c = (fun channel ~src ~params ~references ~tgt ->
                Printf.fprintf channel "
%s = caml_alloc(%s->%s, 0);
CAMLlocal1(field);
for (%s i = 0; i < %s->%s; i++) {
  %t
  Store_field(%s, i, field);
}
" tgt src length (Clang.get_type_spelling length_ty) src length
(fun channel -> contents_type_info.ocaml_of_c channel ~src:(Printf.sprintf "%s->%s[i]" src contents) ~params:[| |] ~references ~tgt:"field")
tgt); }, Regular
        | None -> default_type (Clang.get_type_spelling ty)
        | _ -> assert false
      end
  | ConstantArray ->
      begin
        let element = Clang.get_array_element_type ty in
        let size = Clang.get_array_size ty in
        let element_type_info, _ = find_type_info ~declare_abstract context empty_type_interface element in
        { ocamltype = Ppxlib.Ast_helper.Typ.tuple (List.init size (fun _ -> element_type_info.ocamltype));
          c_of_ocaml = (fun channel ~src ~params ~references ~tgt ->
                Printf.fprintf channel "
CAMLlocal1(ocaml_field);
for (size_t i = 0; i < %d; i++) {
  %s field;
  ocaml_field = Field(%s, i);
  %t
  %s[i] = field;
}
" size (Clang.get_type_spelling element) src (fun channel -> element_type_info.c_of_ocaml channel ~src:"ocaml_field" ~params:[| |] ~references ~tgt:"field") tgt);
          ocaml_of_c = (fun channel ~src ~params ~references ~tgt ->
                Printf.fprintf channel "
%s = caml_alloc_tuple(%d);
CAMLlocal1(field);
for (size_t i = 0; i < %d; i++) {
  %t
  Store_field(%s, i, field);
}
" tgt size size (fun channel -> element_type_info.ocaml_of_c channel ~src:(Printf.sprintf "%s[i]" src) ~params:[| |] ~references ~tgt:"field") tgt)}, Regular
      end
  | _ ->
      match Clang.ext_get_type_kind ty with (* for Ocaml 3.8 *)
      | Elaborated ->
          let full_type_name = Clang.get_type_spelling ty in
          begin
            match get_elaborated_type full_type_name with
            | None -> failwith full_type_name
            | Some (Enum type_name) -> find_enum_info type_name
            | Some (Struct type_name) ->
                let struct_info =
                  try
                    String_hashtbl.find context.struct_table type_name
                  with Not_found ->
                    failwith ("Unknown struct " ^ type_name) in
                let common_info, struct_info = struct_info in
                Lazy.force common_info, Struct struct_info
          end
      | _ ->
          match type_name with
          | "uint64_t" | "int64_t" ->
              int64_type_info
          | _ ->
              default_type type_name in
  if type_interface.null_is_none then
    let common_info =
      { ocamltype = ocaml_option common_info.ocamltype;
        c_of_ocaml = (fun channel ~src ~params ~references ~tgt ->
          Printf.fprintf channel "\
  if (Is_long(%s)) {
    %s = NULL;
  }
  else {
    CAMLlocal1(option_value);
    option_value = Field(%s, 0);
    %t
  };
" src tgt src (common_info.c_of_ocaml ~src:"option_value" ~params ~references ~tgt));
        ocaml_of_c = (fun channel ~src ~params ~references ~tgt ->
          Printf.fprintf channel "\
  if (%s == NULL) {
    %s = Val_int(0);
  }
  else {
    CAMLlocal1(option_value);
    %t
    %s = caml_alloc(1, 0);
    Store_field(%s, 0, option_value);
  };
" src tgt (common_info.ocaml_of_c ~src ~params ~references ~tgt:"option_value") tgt tgt) }
    in
    common_info, type_info
  else
    common_info, type_info

let make_tuple list =
  match list with
  | [] -> Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "unit")) []
  | [ty] -> ty
  | _ -> Ppxlib.Ast_helper.Typ.tuple list

type 'a output = {
    desc : 'a;
    on_success : bool;
    on_error : bool;
  }

let desc_on_success list =
  list |> List.filter (fun o -> o.on_success) |> List.map (fun o -> o.desc)

let desc_on_error list =
  list |> List.filter (fun o -> o.on_error) |> List.map (fun o -> o.desc)

let translate_type_info ?(outputs = []) (common_info, type_info) =
  match type_info with
  | Void -> make_tuple (desc_on_success outputs)
  | Regular -> make_tuple (common_info.ocamltype :: (desc_on_success outputs))
  | Bool | Not_bool ->
      if outputs = [] then
        bool_info.ocamltype
      else
        ocaml_option (make_tuple (desc_on_success outputs))
  | Enum enum_info ->
      let ocaml_type =
        match enum_info.result with
        | Some _ ->
            Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "result"))
              [make_tuple (desc_on_success outputs); make_tuple (common_info.ocamltype :: desc_on_error outputs)]
        | None -> common_info.ocamltype in
      ocaml_type
  | Struct struct_info ->
      make_tuple (common_info.ocamltype :: desc_on_success outputs)

let translate_type ?outputs ?declare_abstract ?parameters context type_interface ty =
  translate_type_info ?outputs
    (find_type_info ?declare_abstract ?parameters context type_interface ty)

type field_type =
  | Unknown of string * Clang.cxtype
  | Translated of string * Clang.cxtype * (common_type_info * type_info)
  | Sized_string of
      { length : string * Clang.cxtype; contents : string * Clang.cxtype }

let rec list_chop p list =
  match list with
  | [] -> raise Not_found
  | hd :: tl ->
      match p hd with
      | Some hd -> hd, tl
      | None ->
          let item, tl = list_chop p tl in
          item, hd :: tl

let rec list_mutate f list =
  match list with
  | [] -> raise Not_found
  | hd :: tl ->
      match f hd with
      | None -> hd :: list_mutate f tl
      | Some hd -> hd :: tl

let print_list chan list =
  let first = ref true in
  List.iter (fun arg ->
    if !first then
      first := false
    else
      Printf.fprintf chan ", ";
    Printf.fprintf chan "%s" arg) list

let print_ocaml_primitive channel name args print_body =
 Printf.fprintf channel "CAMLprim value\n%s(%a)\n{%t}\n\n"
    name print_list args print_body

type 'a argument_value =
  | Fixed of string
  | Variable of 'a

let map_argument_value f x =
  match x with
  | Fixed s -> Fixed s
  | Variable v -> Variable (f v)

let reduce_argument_value f x =
  match x with
  | Fixed s -> s
  | Variable v -> f v

type output_desc =
  | Info of common_type_info * type_info * string array
  | Regular of Clang.cxtype
  | Sized_string of string
  | Array of string * Clang.cxtype argument_value * Clang.cxtype

let print_return_ocaml_of_c context used_arg_names print_expression
    result_type result_type_interface ?result_name (common_info, type_info) ~params ~references outputs =
  let print_output channel src output type_interface tgt =
    match output with
    | Info (common_info, type_info, params) ->
        common_info.ocaml_of_c channel ~src ~params ~references ~tgt
    | Regular ty ->
        let common_info, type_info =
          find_type_info context type_interface ty in
        common_info.ocaml_of_c channel ~src ~params:[| |] ~references ~tgt
    | Sized_string length ->
        Printf.fprintf channel "%s = caml_alloc_initialized_string(%s, %s);\n"
          tgt length src
    | Array (length, length_ty, cell_ty) ->
        let common_info, type_info =
          find_type_info context empty_type_interface cell_ty in
        Printf.fprintf channel
          "%s = caml_alloc(%s, 0);
CAMLlocal1(cell);
for (%s i = 0; i < %s; i++) {
  %t
  Store_field(%s, i, cell);
}
" tgt length (reduce_argument_value Clang.get_type_spelling length_ty) length
(fun channel -> common_info.ocaml_of_c channel ~src:(Printf.sprintf "%s[i]" src) ~params:[| |] ~references ~tgt:"cell")
tgt
 in
  let make_data outputs channel =
    begin
      match outputs with
      | [] -> Printf.fprintf channel "data = Val_unit;"
      | [single, output, type_interface] ->
          print_output channel single output type_interface "data"
      | list ->
          Printf.fprintf channel "data = caml_alloc_tuple(%d);\n"
            (List.length list);
          list |> List.iteri @@ fun i (s, output, type_interface) ->
            Printf.fprintf channel "  {
    CAMLlocal1(field);
    %t
    Store_field(data, %d, field);
  }\n"
              (fun channel -> print_output channel s output type_interface "field")
              i
    end;
    outputs |> List.iter @@ fun (s, output, type_interface) ->
      match type_interface.destructor with
      | None -> ()
      | Some destructor -> output_string channel (destructor s) in
  if Clang.get_type_kind result_type = Void then
    if outputs = [] then
      begin
        Printf.fprintf context.chan_stubs "\n  %t;" print_expression;
        Printf.fprintf context.chan_stubs "\n  CAMLreturn(Val_unit);\n"
      end
    else
      begin
        Printf.fprintf context.chan_stubs "
  %t;
  {
    CAMLlocal1(data);
    %t
    CAMLreturn(data);
  }
" print_expression (make_data (outputs |> List.filter (fun (_, o, _) -> o.on_success) |> List.map (fun (s, o, i) -> (s, o.desc, i))))
      end
  else
    begin
      let result = make_name_unique used_arg_names "result" in
      Printf.fprintf context.chan_stubs "\n  %s %s = %t;"
        (Clang.get_type_spelling result_type) result print_expression;
      match type_info with
      | Bool | Not_bool when outputs <> [] ->
          let prefix =
            if type_info = Bool then ""
            else "!" in
          Printf.fprintf context.chan_stubs "
  if (%s%s) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    %t
    Store_field(ocaml_result, 0, data);
    %s
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLreturn(Val_int(0));
  }" prefix result (make_data (outputs |> List.filter (fun (_, o, _) -> o.on_success) |> List.map (fun (s, o, i) -> (s, o.desc, i))))
          (match result_type_interface.destructor with None -> ""
               | Some destructor -> destructor result)
      | Enum { result = Some success } ->
          let real_result = match result_name with Some result -> result | None -> result in
          Printf.fprintf context.chan_stubs "
  if (%s == %s) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    %t
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    %t
    Store_field(ocaml_result, 0, data);
    %s
    CAMLreturn(ocaml_result);
  }" real_result success (make_data (outputs |> List.filter (fun (_, o, _) -> o.on_success) |> List.map (fun (s, o, i) -> (s, o.desc, i)))) (make_data ((real_result, Info (common_info, type_info, params), result_type_interface) :: (outputs |> List.filter (fun (_, o, _) -> o.on_error) |> List.map (fun (s, o, i) -> (s, o.desc, i)))))
          (match result_type_interface.destructor with None -> ""
               | Some destructor -> destructor result)
      | _ ->
          Printf.fprintf context.chan_stubs "
  {
    CAMLlocal1(data);
    %t
    CAMLreturn(data);
  }\n"
(make_data ((result, Info (common_info, type_info, params), result_type_interface) :: (outputs |> List.filter (fun (_, o, _) -> o.on_success) |> List.map (fun (s, o, i) -> (s, o.desc, i)))))
    end

let print_return_c_of_ocaml context used_arg_names print_expression
    result_type_interface result_type =
  if Clang.get_type_kind result_type = Void then
    begin
      Printf.fprintf context.chan_stubs "  %t;" print_expression;
    end
  else
    begin
      let result = "result" in
      Printf.fprintf context.chan_stubs "  %s = %t;\n" result print_expression;
      let common_info, type_info =
        find_type_info context result_type_interface result_type in
      Printf.fprintf context.chan_stubs "  \
  {
    CAMLlocal1(data);
    %t
    CAMLreturnT(%s, data);
  }
"
        (fun channel -> common_info.c_of_ocaml channel ~src:result ~params:[| |] ~references:[| |] ~tgt:"data")
        (Clang.get_type_spelling result_type)
    end

let make_destructor context type_name ocaml_name type_interface =
  match type_interface.destructor with
  | None -> "custom_finalize_default"
  | Some destructor ->
      let finalizer = "finalize_" ^ ocaml_name in
      Printf.fprintf context.chan_stubs "\
static void %s(value v) {
  %s;
}
" finalizer (destructor (Printf.sprintf "*((%s *) Data_custom_val(v))" type_name));
      finalizer

let declare_opaque context type_name ocaml_type_name type_interface =
  let destructor = make_destructor context type_name ocaml_type_name type_interface in
  let compare =
    match type_interface.compare with
    | None -> "custom_compare_default"
    | Some compare -> compare in
  let hash =
    match type_interface.hash with
    | None -> "custom_hash_default"
    | Some hash -> hash in
  Printf.fprintf context.chan_stubs
    "DECLARE_OPAQUE_EX(%s, %s, %s, %s, %s, %s, %s)\n\n"
    type_name ocaml_type_name (name_of_c_of_ocaml ocaml_type_name)
    (name_of_ocaml_of_c ocaml_type_name)
    destructor compare hash

let deriving_attr () =
  Metapp.Attr.mk (Metapp.mkloc "deriving") (PStr [%str refl])

let translate_struct_decl' context cur typedef name =
  let interface = get_struct name context.module_interface in
  let type_interface = get_type name context.module_interface in
  let ocaml_type_name = make_ocaml_type_name name in
  let fields_ref = ref [] in
  ignore (Clang.visit_children cur (fun cur par ->
    begin
      match Clang.get_cursor_kind cur with
      | FieldDecl ->
          let name = Clang.get_cursor_spelling cur in
          let ty = Clang.get_cursor_type cur in
          fields_ref := (name, (ty, cur)) :: !fields_ref
      | _ -> ()
    end;
    Continue));
  let fields = List.rev !fields_ref in
  let ocaml_fields_ref =
    ref (fields |>List.map (fun (name, (ty, cur)) -> Unknown (name, ty), cur)) in
  let apply_field_rule (rule : field_interface) =
    match rule with
    | Sized_string { length; contents } ->
        let length, fields = !ocaml_fields_ref |> list_chop (fun (field, _) ->
          match field with
          | Unknown (name, ty) when name = length -> Some (name, ty)
          | _ -> None) in
        ocaml_fields_ref := fields |> list_mutate (fun (field, cur) ->
          match field with
          | Unknown (name, ty) when name = contents ->
              Some (Sized_string {length; contents = (name, ty)}, cur)
          | _ -> None) in
  List.iter apply_field_rule interface.fields;
  let ocaml_fields = !ocaml_fields_ref in
  let common_info = make_common_type_info ~type_interface ocaml_type_name in
  let recognize_type (field_type, cur) =
    match field_type with
    | Unknown (name, ty) ->
        Translated (name, ty,
          find_type_info ~declare_abstract:false context empty_type_interface ty), cur
    | ty -> ty, cur in
  let record_fields =
    lazy (
      try Some (List.map recognize_type ocaml_fields)
      with Unknown_type -> None) in
  let make_decl () =
    let type_name =
      if typedef then name
      else Printf.sprintf "struct %s" name in
    let (kind : Ppxlib.type_kind), manifest =
      match Lazy.force record_fields with
      | None ->
          declare_opaque context type_name ocaml_type_name type_interface;
          Ppxlib.Ptype_abstract, None
      | Some fields ->
          let nb_fields = List.length fields in
          let val_field tgt channel field =
            match field with
            | Unknown _ -> assert false
            | Sized_string { contents = (contents, _); length = (length, _) } ->
                Printf.fprintf channel "\
    %s = caml_alloc_initialized_string(v.%s, v.%s);"
                  tgt length contents
            | Translated (name, _ty, (common_type_info, type_info)) ->
                common_type_info.ocaml_of_c channel ~src:(Printf.sprintf "v.%s" name) ~params:[| |] ~references:[| |] ~tgt in
          let field_val src channel field =
            match field with
            | Unknown _ -> assert false
            | Sized_string { contents = (contents, _); length = (length, _) } ->
                Printf.fprintf context.chan_stubs "\
  v.%s = caml_string_length(%s);
  v.%s = String_val(%s);
" length src contents src
            | Translated (name, ty, (common_type_info, type_info)) ->
               common_type_info.c_of_ocaml context.chan_stubs ~src ~params:[| |] ~references:[| |] ~tgt:(Printf.sprintf "v.%s" name) in
          let field_name field =
            match field with
            | Unknown _ -> assert false
            | Sized_string { contents = (name, _) }
            | Translated (name, _, _) -> name in
          let translate_field_type field =
            match field with
            | Unknown _ -> assert false
            | Sized_string { contents = (_name, _) } -> ocaml_string
            | Translated (_name, _, ty) -> translate_type_info ty in
          match fields with
          | [field, _] ->
              Printf.fprintf context.chan_stubs "\
static value __attribute__((unused))
%s(%s v)
{
  CAMLparam0();
  CAMLlocal1(data);
  %a
  CAMLreturn(data);
}

static %s __attribute__((unused))
%s(value ocaml)
{
  CAMLparam1(ocaml);
  %s v;
  %a
  CAMLreturnT(%s, v);
}
" (name_of_ocaml_of_c ocaml_type_name) type_name (val_field "data") field
   type_name (name_of_c_of_ocaml ocaml_type_name) type_name
   (field_val "ocaml") field type_name;
              Ptype_abstract, Some (translate_field_type field)
          | _ ->
              Printf.fprintf context.chan_stubs "\
static value __attribute__((unused))
%s(%s v)
{
  CAMLparam0();
  CAMLlocal1(ocaml);
  ocaml = caml_alloc_tuple(%d);
" (name_of_ocaml_of_c ocaml_type_name) type_name nb_fields;
          fields |> List.iteri (fun i (field, _) ->
                Printf.fprintf context.chan_stubs "\
  {
     CAMLlocal1(data);
     %a
     Store_field(ocaml, %d, data);
  }
" (val_field "data") field i);
              Printf.fprintf context.chan_stubs "\
  CAMLreturn(ocaml);
}

static %s __attribute__((unused))
%s(value ocaml)
{
  CAMLparam1(ocaml);
  %s v;
" type_name (name_of_c_of_ocaml ocaml_type_name) type_name;
          fields |> List.iteri (fun i (field, _) ->
            field_val (Printf.sprintf "Field(ocaml, %d)" i) context.chan_stubs field);
          Printf.fprintf context.chan_stubs "\
  CAMLreturnT(%s, v);
}
" type_name;
          let fields = fields |> List.map @@ fun (field, cur) ->
            let attrs = make_doc_attributes cur in
            Ppxlib.Ast_helper.Type.field (loc (String.lowercase_ascii (field_name field)))
              (translate_field_type field) ~attrs in
                Ptype_record fields, None in
    let attrs = make_doc_attributes cur in
    let attrs =
      match kind with
      | Ptype_abstract -> attrs
      | _ -> deriving_attr () :: attrs in
    let type_decl =
      Ppxlib.Ast_helper.Type.mk (loc ocaml_type_name) ~kind ~attrs ?manifest in
    add_sig_type context [type_decl] in
  let decl_made = ref false in
  let make_decl () =
    if not !decl_made then
      begin
        decl_made := true;
        make_decl ()
      end in
  if typedef then
    String_hashtbl.add context.type_table name
      (lazy (make_decl (); common_info), Struct ())
  else
    String_hashtbl.add context.struct_table name
      (lazy (make_decl (); common_info), ());
  String_hashtbl.add context.used_type_table ocaml_type_name ();
  let used_arg_names = String_hashtbl.create 17 in
  let print_accessor { field_name; accessor_name; stub_name } =
    make_decl ();
    let field_type, _ = List.assoc field_name fields in
    print_ocaml_primitive context.chan_stubs stub_name ["value arg_ocaml"]
      (fun channel ->
         Printf.fprintf channel "
CAMLparam1(arg_ocaml);
%s arg;\n" name;
         common_info.c_of_ocaml channel
                ~src:"arg_ocaml"
                ~params:[| |] ~references:[| |]
                ~tgt:"arg";
         let print_expression channel =
           Printf.fprintf channel "arg.%s"
             field_name in
         let field_type_info =
           find_type_info context empty_type_interface field_type in
         let print_result channel =
           print_return_ocaml_of_c context used_arg_names print_expression
             field_type empty_type_interface field_type_info ~params:[| |] ~references:[| |] [] in
         Printf.fprintf channel "
  %t" print_result);
    let prim = [stub_name] in
    let field_type_info, _ =
      find_type_info context empty_type_interface field_type in
    let pval_type = Ppxlib.Ast_helper.Typ.arrow Nolabel common_info.ocamltype
      field_type_info.ocamltype in
    let desc = Ppxlib.Ast_helper.Val.mk (loc accessor_name) pval_type ~prim in
    add_primitive context desc in
  List.iter print_accessor interface.accessors

let translate_struct_decl context cur =
  let name = Clang.get_cursor_spelling cur in
  let typedef, name =
    if name = "" then
      true, Clang.get_type_spelling (Clang.get_cursor_type cur)
    else false, name in
  translate_struct_decl' context cur typedef name

let longuest_common_prefix s0 s1 =
  let up_to = min (String.length s0) (String.length s1) in
  let rec check previous i =
    if i < up_to then
      let j = succ i in
      let prefix = String.sub s0 0 j in
      if prefix = String.sub s1 0 j then
        check prefix j
      else
        previous
    else
      previous in
  check "" 0

module Int_hashtbl = Hashtbl.Make (struct
  type t = int

  let equal = ( = )

  let hash = Hashtbl.hash
end)

let translate_enum_decl context cur =
  let name = Clang.get_cursor_spelling cur in
  let interface = get_enum name context.module_interface in
  let type_interface = get_type name context.module_interface in
  let typedef, name =
    if name = "" then
      true, Clang.get_type_spelling (Clang.get_cursor_type cur)
    else false, name in
  let ocaml_type_name = make_ocaml_type_name name in
  let result = ref None in
  let constructors_ref = ref [] in
  let already_bound = Int_hashtbl.create 17 in
  ignore (Clang.visit_children cur (fun cur par ->
    begin
      match Clang.get_cursor_kind cur with
      | EnumConstantDecl ->
          let name = Clang.get_cursor_spelling cur in
          let interface = get_constant name interface in
          let value = Clang.get_enum_constant_decl_value cur in
          if not (Int_hashtbl.mem already_bound value) ||
            (if interface.preferred then
               begin
                 constructors_ref := List.filter (fun (_, (v, _)) -> v <> value) !constructors_ref;
                 true
               end
            else
              false) then
            begin
              Int_hashtbl.add already_bound value ();
              if interface.success then
                result := Some name
              else
                constructors_ref := (name, (value, cur)) :: !constructors_ref
            end
      | _ -> ()
    end;
    Continue));
  let result = !result in
  let constructors = List.rev !constructors_ref in
  let longuest_common_prefix =
    match List.map fst constructors with
    | [] -> ""
    | hd :: tl -> List.fold_left longuest_common_prefix hd tl in
  let constructors =
    match String.rindex_opt longuest_common_prefix '_' with
    | None -> List.map (fun (name, value) -> (name, name, value)) constructors
    | Some index ->
        List.map (fun (name, value) ->
          name, String.sub name (index + 1) (String.length name - index - 1), value)
          constructors in
  let ocaml_constructors =
    List.map (fun (_, name, (_, cur)) ->
      let attrs = make_doc_attributes cur in
      Ppxlib.Ast_helper.Type.constructor (loc (String.capitalize_ascii name)) ~attrs)
      constructors in
  let common_info = make_common_type_info ~type_interface ocaml_type_name in
  let enum_info = { result; constructors = List.map (fun (a, b, (c, _)) -> (a, b, c)) constructors } in
  let first_constructor =
    match constructors with
    | [] -> failwith ("Empty enum: " ^ name)
    | hd :: _ -> hd in
  let make_decl () =
    let type_name =
      if typedef then name
      else Printf.sprintf "enum %s" name in
    Printf.fprintf context.chan_stubs
      "%s\n%s(value ocaml)\n{\n  switch (Int_val(ocaml)) {\n"
      type_name (name_of_c_of_ocaml ocaml_type_name);
    constructors |> List.iteri (fun i (constructor, _, _) ->
      Printf.fprintf context.chan_stubs
        "  case %i: return %s;\n" i constructor);
    Printf.fprintf context.chan_stubs
      "  }
  failwith_fmt(\"invalid value for %s: %%d\", Int_val(ocaml));
  return %s;
}\n\n"
      (name_of_c_of_ocaml ocaml_type_name) (match first_constructor with (name, _, _) -> name);
    Printf.fprintf context.chan_stubs
      "value\n%s(%s v)\n{\n  switch (v) {\n"
      (name_of_ocaml_of_c ocaml_type_name) type_name;
    constructors |> List.iteri (fun i (constructor, _, _) ->
      Printf.fprintf context.chan_stubs
        "  case %s: return Val_int(%i);\n" constructor i);
    begin
      match result with
      | None -> ()
      | Some result ->
          Printf.fprintf context.chan_stubs
            "  case %s: failwith(\"unexpected success value\");\n"
            result
    end;
    Printf.fprintf context.chan_stubs
      "  }
  failwith_fmt(\"invalid value for %s: %%d\", v);
  return Val_int(0);
}\n\n"
      (name_of_ocaml_of_c ocaml_type_name);
    let doc_attributes = make_doc_attributes cur in
    let type_decl =
      Ppxlib.Ast_helper.Type.mk ~kind:(Ptype_variant ocaml_constructors)
        ~attrs:(deriving_attr () :: doc_attributes @ interface.attributes)
        (loc ocaml_type_name) in
    add_sig_type context [type_decl] in
  if typedef then
    String_hashtbl.add context.type_table name
      (lazy (make_decl (); common_info), Enum enum_info)
  else
    String_hashtbl.add context.enum_table name
      (lazy (make_decl (); common_info), enum_info);
  String_hashtbl.add context.used_type_table ocaml_type_name ()

let translate_typedef_decl context cur =
  let name = Clang.get_cursor_spelling cur in
  let underlying_type = Clang.get_typedef_decl_underlying_type cur in
  if
    match Clang.ext_get_type_kind underlying_type, (* for Ocaml 3.8 *)
      get_elaborated_type (Clang.get_type_spelling underlying_type) with
    | Elaborated, Some (Enum type_name) ->
        begin
          match String_hashtbl.find_opt context.enum_table type_name with
          | None -> true
          | Some enum_info ->
              String_hashtbl.add context.type_table name
                (
                  let common_info, enum_info = enum_info in
                  common_info, Enum enum_info);
              false
        end
    | Elaborated, Some (Struct type_name) ->
        begin
          match String_hashtbl.find_opt context.struct_table type_name with
          | None -> true
          | Some struct_info ->
              String_hashtbl.add context.type_table name
                (
                  let common_info, struct_info = struct_info in
                  common_info, Struct struct_info);
              false
        end
    | _ ->
        true
  then
    if not (String_hashtbl.mem context.type_table name) then
    begin
      let type_interface = get_type name context.module_interface in
      let ocaml_type_name = make_ocaml_type_name name in
      let common_info = make_common_type_info ~type_interface ocaml_type_name in
      let make_decl () =
        declare_opaque context name ocaml_type_name type_interface;
        let type_decl = Ppxlib.Ast_helper.Type.mk (loc ocaml_type_name) in
        add_sig_type context [type_decl] in
      String_hashtbl.add context.type_table name
        (lazy (make_decl (); common_info), Regular);
      String_hashtbl.add context.used_type_table ocaml_type_name ()
    end

type argument_type =
  | Removed of argument_type
  | Removed_output of argument_type
  | Output of { output_type : argument_type; on_success : bool; on_error : bool }
  | Fixed_value of string
  | CXType of Clang.cxtype
  | Update of Clang.cxtype
  | Array of int argument_value * Clang.cxtype argument_value * Clang.cxtype
  | Sized_string of int argument_value * Clang.cxtype argument_value * Clang.cxtype
  | Closure of {
      data_caller : int;
      data_caller_type : Clang.cxtype;
      closure_args : (string * Clang.cxtype) array;
      closure_result : Clang.cxtype;
      data_callee : int;
      references : int list;
    }

let find_argument (argument : argument) index_args =
  match argument with
  | Index i -> i
  | Name name ->
      try
        String_hashtbl.find index_args name
      with Not_found ->
        failwith ("HERE: " ^ name)

let translate_argument_type context type_interface ty =
  match ty with
  | Removed _ | Removed_output _ | Output _ | Fixed_value _ -> assert false
  | CXType ty | Update ty -> translate_type context type_interface ty
  | Array (_, _, ty) ->
      let contents =
        translate_type context type_interface (Clang.get_pointee_type ty) in
      ocaml_array contents
  | Sized_string _ -> ocaml_string
  | Closure { closure_args; closure_result; data_callee } ->
      let rec build_closure_type (_, arg_type) (i, accu) =
        let j = pred i in
        if j = data_callee then
          j, accu
        else
          j, Ppxlib.Ast_helper.Typ.arrow Nolabel (translate_type context empty_type_interface arg_type) accu in
      snd (Array.fold_right build_closure_type closure_args
        (Array.length closure_args, translate_type context empty_type_interface closure_result))

let index_args args =
  let index = String_hashtbl.create 17 in
  args |> Array.iteri (fun i arg -> String_hashtbl.add index arg i);
  index

let rec get_argument_type ty =
  match ty with
  | Removed ty -> get_argument_type ty
  | Removed_output ty -> get_argument_type ty
  | CXType ty | Update ty -> ty
  | _ -> failwith "get_argument_type"

let array_find p a =
  let rec aux i =
    if i < Array.length a then
      if p (Array.unsafe_get a i) then
        Some i
      else
        aux (succ i)
    else
      None in
  aux 0

let find_carried_reference name carry_reference args module_interface =
  match carry_reference with
  | None -> []
  | Some ty ->
    match args |> array_find begin fun i ->
      match i with CXType ty' -> Clang.get_type_spelling ty' = ty | _ -> false
    end with
    | Some index -> [(index, fun x -> x)]
    | None ->
        match args |> array_find begin fun i -> match i with
        | CXType ty' ->
          begin
            let type_interface = get_type (Clang.get_type_spelling ty') module_interface in
            match type_interface.carry_reference with
            | None -> false
            | Some ty' -> ty = ty'
          end
        | _ ->
            false
        end with
        | Some index ->
            [(index, fun x -> Printf.sprintf "safe_field(%s, 1)" x)]
        | None ->
            match args |> array_find (fun i -> match i with CXType ty' ->
              begin
                let type_interface = get_type (Clang.get_type_spelling ty') module_interface in
                match type_interface.carry_reference with
                | None -> false
                | Some ty' ->
                    let type_interface = get_type ty' module_interface in
                    match type_interface.carry_reference with
                    | None -> false
                    | Some ty' -> ty = ty'
              end
            | _ ->
                false) with
            | Some index ->
                [(index, fun x -> Printf.sprintf "safe_field(safe_field(%s, 1), 1)" x)]
            | None ->
                Printf.fprintf stderr "warning: reference not found in %s\n" name;
                []

let get_value (value : value) args index : int argument_value * Clang.cxtype argument_value =
  match value with
  | Fixed (value, ty) -> Fixed (string_of_int value), Fixed ty
  | Argument argument ->
      let value = find_argument argument index in
      Variable value, Variable (get_argument_type args.(value))

let remove_args args value =
  match value with
  | Variable value -> args.(value) <- Removed args.(value)
  | Fixed _ -> ()

let remove_output_args args value =
  match value with
  | Variable value -> args.(value) <- Removed_output args.(value)
  | Fixed _ -> ()

let get_arg args value =
  match value with
  | Variable index -> args.(index)
  | Fixed value -> value

let detect_closures args : argument_interface option =
  let pointer = ref None in
  let data_caller = ref None in
  let data_callee = ref None in
  let references = ref [] in
  let detect_callee_arg i (parameter : Clang.Lazy.Ast.parameter) =
    match Lazy.force ((Lazy.force parameter.desc).qual_type).desc with
    | Pointer { desc = lazy (BuiltinType Void); _ } ->
        data_callee := Some (Index i)
    | _ -> () in
  let detect_arg i ty =
    match Lazy.force (Clang.Lazy.Type.of_cxtype ty).desc with
    | Typedef { name = IdentifierName "CXCursor"; _ } ->
        references := Index i :: !references
    | Pointer { desc = lazy (BuiltinType Void); _ } ->
        data_caller := Some (Index i)
    | Pointer { desc = lazy (FunctionType {
          parameters = Some { non_variadic; _ }; _ })} ->
        pointer := Some (Index i);
        non_variadic |> List.iteri detect_callee_arg
    | _ -> () in
  args |> Array.iteri detect_arg;
  match !pointer, !data_caller, !data_callee with
  | Some pointer, Some data_caller, Some data_callee ->
      Some (Closure {
        pointer; data_caller; data_callee; references = !references })
  | _ -> None

let translate_function_decl context cur =
  let name = Clang.get_cursor_spelling cur in
  let function_interface = get_function name context.module_interface in
  if not (function_interface.hidden) then
  let pval_name = loc (function_interface.rename name) in
  let ty = Clang.get_cursor_type cur in
  let num_args = Clang.cursor_get_num_arguments cur in
  let arg_names = Array.init num_args (fun i ->
    Clang.get_cursor_spelling (Clang.cursor_get_argument cur i)) in
  let index = index_args arg_names in
  let args = Array.init num_args (fun i -> Clang.get_arg_type ty i) in
  let outputs = ref [] in
  let arg_interfaces = Array.make num_args empty_type_interface in
  let argument_interfaces = function_interface.arguments in
  let argument_interfaces =
    match detect_closures args with
    | None -> argument_interfaces
    | Some argument_interface -> argument_interface :: argument_interfaces in
  let args = Array.map (fun ty -> CXType ty) args in
  let apply_argument_rule (rule : argument_interface) =
    match rule with
    | Array { length; contents } ->
        let length, length_ty = get_value length args index in
        let contents = find_argument contents index in
        let contents_ty = get_argument_type args.(contents) in
        args.(contents) <- Array (length, length_ty, contents_ty);
        remove_args args length
    | Sized_string { length; contents } ->
        let length, length_ty = get_value length args index in
        let contents = find_argument contents index in
        let contents_ty = get_argument_type args.(contents) in
        args.(contents) <- Sized_string (length, length_ty, contents_ty);
        remove_args args length
    | Output { argument; on_success; on_error } ->
        let output = find_argument argument index in
        begin
          match args.(output) with
          | CXType ty ->
              let ty = Clang.get_pointee_type ty in
              outputs := { desc = (Some output, find_type_info context empty_type_interface ty); on_success; on_error } :: !outputs;
              args.(output) <- Output { output_type = CXType ty; on_success; on_error }
          | Sized_string (length, length_ty, contents_ty) ->
              let length_ty = map_argument_value Clang.get_pointee_type length_ty in
              let contents_ty = Clang.get_pointee_type contents_ty in
              outputs := { desc = (Some output, string_type_info); on_success; on_error } :: !outputs;
              args.(output) <- Output { output_type = Sized_string (length, length_ty, contents_ty); on_success = true; on_error = false };
              remove_output_args args length
          | Array (length, length_ty, contents_ty) ->
              let length_ty = map_argument_value Clang.get_pointee_type length_ty in
              let contents_ty = Clang.get_pointee_type contents_ty in
              let type_interface =
                empty_type_interface |> reinterpret_as (Sized_array { length = Fixed (2, "int") }) in
              outputs := { desc = (Some output, find_type_info context type_interface contents_ty); on_success; on_error } :: !outputs;
              args.(output) <- Output { output_type = Array (length, length_ty, contents_ty); on_success = true; on_error = false };
              remove_output_args args length
          | _ -> failwith "Argument expected (Output)"
        end
    | Update output ->
        let output = find_argument output index in
        begin
          match args.(output) with
          | CXType ty ->
              let ty = Clang.get_pointee_type ty in
              outputs := { desc = (Some output, find_type_info context empty_type_interface ty); on_success = true; on_error = false } :: !outputs;
              args.(output) <- Update ty
          | _ -> failwith "Argument expected"
        end
    | Fixed_value { argument; value } ->
        let argument = find_argument argument index in
        args.(argument) <- Fixed_value value
    | Closure { pointer; data_caller; data_callee; references } ->
        let pointer = find_argument pointer index in
        let data_caller = find_argument data_caller index in
        begin
          match args.(pointer), args.(data_caller) with
          | CXType pointer_ty, CXType data_caller_type ->
              let pointer_ty =
                match Clang.get_type_kind pointer_ty with
                | Pointer -> pointer_ty
                | Typedef ->
                    Clang.get_typedef_decl_underlying_type (Clang.get_type_declaration pointer_ty)
                | _ -> assert false in
              let pointee_ty = Clang.get_pointee_type pointer_ty in
              let num_args = Clang.get_num_arg_types pointee_ty in
              let closure_args = Array.init num_args (fun i ->
                let arg_ty = Clang.get_arg_type pointee_ty i in
                (Printf.sprintf "arg%d" i, arg_ty)) in
              let closure_result = Clang.get_result_type pointee_ty in
              let index' = index_args (Array.map fst closure_args) in
              let data_callee = find_argument data_callee index' in
              let references = references |> List.map @@ fun arg ->
                find_argument arg index in
              args.(pointer) <-
                Closure {
                  data_caller; data_caller_type; closure_args; closure_result;
                data_callee; references }
          | _ -> failwith "Argument expected"
        end;
        args.(data_caller) <- Removed args.(data_caller)
    | Type_interface { argument; interface } ->
        let argument = find_argument argument index in
        arg_interfaces.(argument) <- union_type_interfaces arg_interfaces.(argument)
          interface in
  List.iter apply_argument_rule argument_interfaces;
  let outputs = List.rev !outputs in
  let result_type = Clang.get_result_type ty in
  let parameters = ref [| |] in
  let result_type_interface =
    get_type (Clang.get_type_spelling result_type) context.module_interface in
  let result_type_interface =
    union_type_interfaces result_type_interface function_interface.result in
  let references = find_carried_reference name result_type_interface.carry_reference args context.module_interface in
  let result_type_info =
    find_type_info ~parameters context result_type_interface result_type in
  let used_arg_names = String_hashtbl.create 17 in
  let wrapper_arg_names = Array.map (fun arg_name ->
    let arg_name =
      if arg_name = "" then "arg"
      else arg_name in
    make_name_unique used_arg_names arg_name) arg_names in
  let result_type_info, real_outputs, result_name =
    let rec aux first_outputs last_outputs =
      match last_outputs with
      | [] -> result_type_info, outputs, None
      | { on_success = true; on_error = true; desc = (Some i, ((_, (Enum { result = Some field } : type_info)) as type_info)) } :: tl ->
          type_info, List.rev_append first_outputs ({ on_success = true; on_error = false; desc = (None, result_type_info) } :: tl), Some (wrapper_arg_names.(i))
      | hd :: tl ->
          aux (hd :: first_outputs) tl in
    aux [] outputs in
  let params =
    !parameters |> Array.map @@ fun param ->
      let param = find_argument param index in
      let ty =
        match args.(param) with
        | CXType ty -> Clang.get_pointee_type ty
        | _ -> failwith "Argument expected" in
      args.(param) <- Output { output_type = CXType ty; on_success = true; on_error = false };
      param in
  let references =
    references |> Array.of_list in
  let result_ty = translate_type_info ~outputs:(List.map (fun o -> { o with desc = translate_type_info (snd o.desc) }) real_outputs) result_type_info in
  let pval_type =
    if num_args = 0 then
      Ppxlib.Ast_helper.Typ.arrow Nolabel (Ppxlib.Ast_helper.Typ.constr (loc (Longident.Lident "unit")) []) result_ty
    else
      let arg_types = Hashtbl.create 17 in
      let arg_list =
        Array.mapi (fun i arg -> (arg, arg_names.(i), arg_interfaces.(i))) args |>
        Array.to_seq |>
        Seq.filter_map begin fun (arg, name, interface) ->
          match arg with
          | Removed _ | Removed_output _ | Output _ | Fixed_value _ -> None
          | _ ->
              let ty = translate_argument_type context interface arg in
              let unique =
                match Hashtbl.find_opt arg_types ty with
                | Some unique ->
                    unique := false;
                    unique
                | None ->
                    let unique = ref true in
                    Hashtbl.add arg_types ty unique;
                    unique in
              Some (unique, name, ty)
        end |> List.of_seq in
      List.fold_right begin fun (unique, name, ty) pval_type ->
        let label =
          if not function_interface.label_unique || !unique then
            Ppxlib.Asttypes.Nolabel
          else
            Ppxlib.Asttypes.Labelled (Stubgen_common.uncamelcase name) in
        Ppxlib.Ast_helper.Typ.arrow label ty pval_type
      end arg_list result_ty in
  let wrapper_name = name ^ "_wrapper" in
  let ocaml_arg_names = Array.map2 (fun arg arg_name ->
      match arg with
      | Removed _ | Removed_output _ | Output _ | Fixed_value _ -> None
      | _ -> Some (Printf.sprintf "%s_ocaml" arg_name))
      args wrapper_arg_names in
  let ocaml_args = ocaml_arg_names |>
    Array.to_seq |> Seq.filter_map (fun x -> x) |> List.of_seq in
  let nb_args = List.length ocaml_args in
  let rec make_buckets args =
    match args with
    | a0 :: a1 :: a2 :: a3 :: a4 :: ((_ :: _) as tl) ->
        [a0; a1; a2; a3; a4] :: make_buckets tl
    | _ -> [args] in
  let ocaml_args_buckets = make_buckets ocaml_args in
  args |> Array.iteri (fun i arg ->
    match arg with
    | Closure {
        data_caller; data_caller_type; closure_args; closure_result;
        data_callee; references } ->
          let callback_name =
            Printf.sprintf "%s_%s_callback" name wrapper_arg_names.(i) in
          let closure_result_string = Clang.get_type_spelling closure_result in
          let closure_args' = closure_args |>
           Array.mapi (fun i (name, ty) -> if i = data_callee then None else Some (name, ty, name ^ "_ocaml")) in
          let ocaml_args = closure_args'
           |> Array.to_seq |> Seq.filter_map (Option.map (fun (_, _, name) -> name)) |> List.of_seq in
          let local = "result" :: "f" :: ocaml_args in
          let init_args chan =
            closure_args' |> Array.iter @@ Option.iter @@ fun (name, ty, name_ocaml) ->
              let type_interface =
                get_type (Clang.get_type_spelling ty) context.module_interface in
              let arg_info, _ = find_type_info context type_interface ty in
              let references' = find_carried_reference name type_interface.carry_reference (Array.of_list (references |> List.map (fun i -> args.(i)))) context.module_interface in
              flush chan;
              arg_info.ocaml_of_c chan ~src:name ~params:[| |] ~references:(references' |> Array.of_list |> Array.map @@ fun (i, accessor) -> accessor (Printf.sprintf "*((value **)%s)[%d]" (fst closure_args.(data_callee)) (i + 1))) ~tgt:name_ocaml in
          let print_expression channel =
            Printf.fprintf channel "caml_callback%s(%a)"
              (match List.length ocaml_args with 1 -> "" | n -> string_of_int n)
              print_list ("f" :: ocaml_args) in
          let print_call_and_return channel =
            print_return_c_of_ocaml context used_arg_names print_expression
              empty_type_interface closure_result in
          Printf.fprintf context.chan_stubs "\
%s
%s(%a)
{
  CAMLparam0();
  CAMLlocal%d(%a);
  f = *((value *) %s);
%t%t
}\n\n"
            closure_result_string
            callback_name print_list
            (closure_args |> Array.to_list |> List.map (fun (name, ty) ->
              Printf.sprintf "%s %s" (Clang.get_type_spelling ty) name))
            (List.length local) print_list local
            (if references = [] then fst closure_args.(data_callee)
             else Printf.sprintf "((value **)%s)[0]" (fst closure_args.(data_callee)))
            init_args print_call_and_return;
          let data_caller_name = Printf.sprintf "&%s_ocaml" wrapper_arg_names.(i) in
          if references = [] then
            wrapper_arg_names.(data_caller) <- data_caller_name
          else
            wrapper_arg_names.(data_caller) <-
              Printf.sprintf "(value *[]){%s,%s}" data_caller_name (String.concat "," (List.map (fun i -> "&" ^ wrapper_arg_names.(i) ^ "_ocaml") references));
          wrapper_arg_names.(i) <- callback_name
    | _ -> ()
  );
  let print_body chan =
    begin
      match ocaml_args_buckets with
      | [] -> assert false
      | hd :: tl ->
          Printf.fprintf context.chan_stubs "\n  CAMLparam%d(%a);"
            (List.length hd) print_list hd;
          tl |> List.iter @@ fun args ->
            Printf.fprintf context.chan_stubs "\n  CAMLxparam%d(%a);"
              (List.length args) print_list args
    end;
    (*
    Printf.fprintf context.chan_stubs "\n    fprintf(stderr, \"%s\\n\");\n" name;
     *)
    args |> Array.iteri (fun i arg ->
      match arg with
      | Removed _ | Removed_output _ | Fixed_value _ -> ()
      | Output { output_type = CXType ty } ->
          Printf.fprintf context.chan_stubs "\n  %s %s;"
            (Clang.get_type_spelling ty) wrapper_arg_names.(i)
      | Output { output_type = Sized_string (length, length_ty, contents_ty) } ->
          Printf.fprintf context.chan_stubs "\n  %s %s;\n  %s %s;"
            (reduce_argument_value Clang.get_type_spelling length_ty) (get_arg wrapper_arg_names length)
            (Clang.get_type_spelling contents_ty) wrapper_arg_names.(i)
      | Output { output_type = Array (length, length_ty, contents_ty) } ->
          Printf.fprintf context.chan_stubs "\n  %s %s;\n  %s %s;"
            (reduce_argument_value Clang.get_type_spelling length_ty) (get_arg wrapper_arg_names length)
            (Clang.get_type_spelling contents_ty) wrapper_arg_names.(i)
      | Output _ -> assert false
      | CXType ty | Update ty ->
          let common_info, type_info = find_type_info context arg_interfaces.(i) ty in
          Printf.fprintf context.chan_stubs "
  %s %s;
  %t"
            (Clang.get_type_spelling ty) wrapper_arg_names.(i)
            (fun channel -> common_info.c_of_ocaml channel
                ~src:(Printf.sprintf "%s_ocaml" wrapper_arg_names.(i))
                ~params:[| |] ~references:[| |]
                ~tgt:wrapper_arg_names.(i))
      | Sized_string (j, length_ty, contents_ty) ->
          Printf.fprintf context.chan_stubs "\n  %s %s = caml_string_length(%s_ocaml);"
            (reduce_argument_value Clang.get_type_spelling length_ty) (get_arg wrapper_arg_names j)
            wrapper_arg_names.(i);
          Printf.fprintf context.chan_stubs "\n  %s %s = String_val(%s_ocaml);"
            (Pcre.replace ~pat:"const" ~templ:"" (Clang.get_type_spelling contents_ty)) wrapper_arg_names.(i) wrapper_arg_names.(i)
      | Array (j, length_ty, contents_ty) ->
          let c_length_ty = reduce_argument_value Clang.get_type_spelling length_ty in
          Printf.fprintf context.chan_stubs "\n  %s %s = Wosize_val(%s_ocaml);"
            c_length_ty (get_arg wrapper_arg_names j)
            wrapper_arg_names.(i);
          let cell_type = Clang.get_pointee_type contents_ty in
          let contents_ty_ast = Clang.Type.of_cxtype contents_ty in
          let nonconst_contents_ty_ast =
            match contents_ty_ast.desc with
            | Pointer qual_type ->
                { contents_ty_ast with
                  desc = Pointer { qual_type with const = false }}
            | _ -> assert false in
          Format.fprintf (Format.formatter_of_out_channel context.chan_stubs)
            "\n  %a %s = xmalloc(%s * sizeof(%s));@."
            Clangml_printer.qual_type nonconst_contents_ty_ast wrapper_arg_names.(i)
            (get_arg wrapper_arg_names j)
            (Clang.get_type_spelling cell_type);
          let common_info, type_info = find_type_info context empty_type_interface cell_type in
          let index = make_name_unique used_arg_names "i" in
          Printf.fprintf context.chan_stubs "\n  %s %s; for (%s = 0; %s < %s; %s++) {\n    %t\n  }"
            c_length_ty index index index (get_arg wrapper_arg_names j) index (fun channel -> common_info.c_of_ocaml channel ~src:(Printf.sprintf "Field(%s_ocaml, %s)" wrapper_arg_names.(i) index) ~params:[| |] ~references:[| |] ~tgt:(Printf.sprintf "%s[%s]" wrapper_arg_names.(i) index))
      | Closure _ -> ());
    let wrapper_args = Array.map2 (fun arg arg_name ->
      match arg with
      | Output _ | Removed_output _ | Update _ -> "&" ^ arg_name
      | Array (_, _, contents_ty) -> arg_name
      | Fixed_value value -> value
      | _ -> arg_name) args wrapper_arg_names in
    let print_expression channel =
      Printf.fprintf channel "%s(%a)" name print_list
        (Array.to_list wrapper_args) in
    print_return_ocaml_of_c context used_arg_names print_expression
      result_type function_interface.result result_type_info ?result_name
      ~params:(params |> Array.map @@ fun i -> wrapper_arg_names.(i))
      ~references:(references |> Array.map @@ fun (i, accessor) -> accessor (Option.get ocaml_arg_names.(i)))
      (real_outputs |> List.map @@ fun { desc = (i, _) } -> match i with None -> "result", { desc = Regular result_type; on_success = true; on_error = false }, empty_type_interface | Some i -> wrapper_arg_names.(i), (match args.(i) with Output { output_type = CXType ty; on_success; on_error } -> { desc = Regular ty; on_success; on_error } | Update ty ->  { desc = Regular ty; on_success = true; on_error = false } | Output { output_type = Sized_string (length, _, _); on_success; on_error } -> { desc = Sized_string (get_arg wrapper_arg_names length); on_success; on_error } | Output { output_type = Array (length, length_ty, contents_ty); on_success; on_error } -> { desc = Array (get_arg wrapper_arg_names length, length_ty, Clang.get_pointee_type contents_ty); on_success; on_error } | _ -> assert false), arg_interfaces.(i)) in
  let ocaml_args_decl =
    ocaml_args |> List.map (fun s -> Printf.sprintf "value %s" s) in
  print_ocaml_primitive context.chan_stubs wrapper_name ocaml_args_decl
    print_body;
  let prim =
    if nb_args <= 5 then
      [wrapper_name]
    else
      let bytecode_name = name ^ "_bytecode" in
      print_ocaml_primitive context.chan_stubs bytecode_name
        ["value *argv"; "int argn"]
        (fun channel ->
          Printf.fprintf channel "\n  return %s(%a);" wrapper_name
            print_list (List.init nb_args
              (fun i -> Printf.sprintf "argv[%d]" i)));
      [bytecode_name; wrapper_name] in
  let attrs = make_doc_attributes cur in
  let desc = Ppxlib.Ast_helper.Val.mk pval_name pval_type ~prim ~attrs in
  add_primitive context desc

let rename_clang name =
  Stubgen_common.uncamelcase (String.sub name 6 (String.length name - 6))

let make_destructor f =
  destructor (fun value -> Printf.sprintf "%s(%s);" f value)

let string_option =
  Type_info ({ ocamltype = ocaml_option ocaml_string;
    c_of_ocaml = (fun _ -> assert false);
    ocaml_of_c = (fun fmt ~src ~params ~references ~tgt ->
      Printf.fprintf fmt "%s = Val_string_option(clang_getCString(%s));
        clang_disposeString(%s);" tgt src src) }, Regular)

let tool_name = "stubgen"

let main cflags llvm_config prefix =
  let clang_options, llvm_version =
    Stubgen_common.prepare_clang_options cflags llvm_config in
  let result_cxerrorcode =
    if llvm_version = Some "3.4.2" then
      integer_zero_is_true
    else
      integer_enum "CXErrorCode" in
  let clang_free =
    match llvm_version with
    | Some llvm_version when llvm_version < "3.7" -> "free"
    | _ -> "clang_free" in
  let module_interface =
    empty_module_interface |>
    add_function (Pcre.regexp "^(?!clang_)|clang_getCString|^clang.*_dispose|^clang_free$|constructUSR|^clang_executeOnThread$|^clang_getDiagnosticCategoryName$|^clang_getDefinitionSpellingAndExtent$|^clang_getToken$|^clang_tokenize$|^clang_annotateTokens$|^clang_.*WithBlock$|^clang_getCursorPlatformAvailability$|^clang_codeComplete|^clang_sortCodeCompletionResults$|^clang_getCompletion(NumFixIts|FixIt)$|^clang_getInclusions$|^clang_remap_getFilenames$|^clang_index.*$|^clang_find(References|Includes)InFile$|^clang_Cursor_getTranslationUnit$|^clang_ext_hash_cursor$|^clang_ext_compare_cursor$") hidden_function_interface |>
    add_type (Pcre.regexp "^CXString$")
      (empty_type_interface |>
       reinterpret_as (Type_info ({ ocamltype = ocaml_string;
                c_of_ocaml = (fun _ -> assert false);
                ocaml_of_c = (fun fmt ~src ~params ~references ~tgt ->
                  Printf.fprintf fmt "%s = caml_copy_string(safe_string(clang_getCString(%s)));
                    clang_disposeString(%s);" tgt src src) }, Regular))) |>
    add_type (Pcre.regexp "^CXInt$")
      (empty_type_interface |>
       make_destructor "clang_ext_Int_dispose") |>
    add_type (Pcre.regexp "^CXFloat$")
      (empty_type_interface |>
       make_destructor "clang_ext_Float_dispose") |>
    add_type (Pcre.regexp "^clang_ext_TemplateName$")
      (empty_type_interface |>
       make_destructor "clang_ext_TemplateName_dispose") |>
    add_type (Pcre.regexp "^clang_ext_TemplateArgument$")
      (empty_type_interface |>
       make_destructor "clang_ext_TemplateArgument_dispose") |>
    add_type (Pcre.regexp "^clang_ext_LambdaCapture$")
      (empty_type_interface |>
       make_destructor "clang_ext_LambdaCapture_dispose") |>
    add_type (Pcre.regexp "^clang_ext_DeclarationName$")
      (empty_type_interface |>
       make_destructor "clang_ext_DeclarationName_dispose") |>
    add_type (Pcre.regexp "^clang_ext_TypeLoc$")
      (empty_type_interface |>
       make_destructor "clang_ext_TypeLoc_dispose") |>
    add_type (Pcre.regexp "^clang_ext_TemplateParameterList$")
      (empty_type_interface |>
       make_destructor "clang_ext_TemplateParameterList_dispose") |>
    add_type (Pcre.regexp "^clang_ext_Requirement$")
      (empty_type_interface |>
       make_destructor "clang_ext_Requirement_dispose") |>
    add_type (Pcre.regexp "^clang_ext_OMPTraitInfo$")
      (empty_type_interface |>
       make_destructor "clang_ext_OMPTraitInfo_dispose") |>
    add_type (Pcre.regexp "^CXIndex$")
      (empty_type_interface |>
       make_destructor "clang_disposeIndex") |>
    add_type (Pcre.regexp "^CXTranslationUnit$")
      (empty_type_interface |>
       make_destructor "clang_disposeTranslationUnit" |>
       carry_reference "CXIndex") |>
    add_type (Pcre.regexp "^clang_ext_NestedNameSpecifierLoc$")
      (empty_type_interface |>
       make_destructor "clang_ext_NestedNameSpecifierLoc_dispose") |>
    add_type (Pcre.regexp "^CXCursor$|^CXType$|^CXFile$|^CXModule$|^CXSourceRange$|^CXSourceLocation$|^CXComment$|^clang_ext_TemplateName$|^clang_ext_TemplateArgument$|^clang_ext_LambdaCapture$|^clang_ext_DeclarationName$|^clang_ext_NestedNameSpecifier$|^clang_ext_NestedNameSpecifierLoc$|^clang_ext_TypeLoc$|^clang_ext_TemplateParameterList$|^clang_ext_Requirement$|^clang_ext_OMPTraitInfo$")
      (empty_type_interface |> carry_reference "CXTranslationUnit") |>
    add_type (Pcre.regexp "^CXToken$")
      (empty_type_interface |> carry_reference "tokens") |>
    add_type (Pcre.regexp "^CXCursor$")
        (empty_type_interface |>
       compare_value "clang_ext_compare_cursor" |>
       hash_value "clang_ext_hash_cursor") |>
    add_type (Pcre.regexp "^CXVirtualFileOverlay$")
      (empty_type_interface |>
       make_destructor "clang_VirtualFileOverlay_dispose") |>
    add_type (Pcre.regexp "^CXModuleMapDescriptor$")
      (empty_type_interface |>
       make_destructor "clang_ModuleMapDescriptor_dispose") |>
    add_type (Pcre.regexp "^CXDiagnosticSet$")
      (empty_type_interface |>
       make_destructor "clang_disposeDiagnosticSet") |>
    add_type (Pcre.regexp "^CXTUResourceUsage$")
      (empty_type_interface |>
       make_destructor "clang_disposeCXTUResourceUsage") |>
    add_enum (Pcre.regexp "^CXCursorKind$")
      (empty_enum_interface |>
       add_constant (Pcre.regexp "^CXCursor_UnexposedExpr$")
        (empty_constant_interface |> set_preferred) |>
       add_constant (Pcre.regexp "^CXCursor_UnexposedAttr$")
        (empty_constant_interface |> set_preferred) |>
       add_constant (Pcre.regexp "^CXCursor_UnexposedStmt$")
        (empty_constant_interface |> set_preferred) |>
       add_constant (Pcre.regexp "^CXCursor_InvalidFile$")
        (empty_constant_interface |> set_preferred) |>
       add_constant (Pcre.regexp "^CXCursor_ObjCSuperClassRef$")
        (empty_constant_interface |> set_preferred)) |>
    add_function (Pcre.regexp "^clang_") (rename_function rename_clang) |>
    add_function (Pcre.regexp "^clang_createTranslationUnitFromSourceFile$")
      (empty_function_interface |>
        add_argument (Array {
          length = Argument (Name "num_clang_command_line_args");
          contents = Name "clang_command_line_args" })) |>
    add_function (Pcre.regexp "^clang_parseTranslationUnit$")
      (empty_function_interface |>
        add_result (empty_type_interface |> null_is_none)) |>
    add_function (Pcre.regexp "^clang_parseTranslationUnit|^clang_indexSourceFile")
      (empty_function_interface |>
        add_argument (Array {
          length = Argument (Name "num_command_line_args");
          contents = Name "command_line_args" })) |>
    add_function (Pcre.regexp "^clang_CXIndex_setGlobalOptions$")
      (empty_function_interface |>
        add_argument (Type_interface {argument = Name "options"; interface = empty_type_interface |> reinterpret_as (Set_of "CXGlobalOptFlags")})) |>
    add_function (Pcre.regexp "^clang_CXIndex_getGlobalOptions$")
      (empty_function_interface |>
        add_result (empty_type_interface |> reinterpret_as (Set_of "CXGlobalOptFlags"))) |>
    add_function (Pcre.regexp "^clang_formatDiagnostic")
      (empty_function_interface |>
        add_argument (Type_interface {argument = Name "Options"; interface = empty_type_interface |> reinterpret_as (Set_of "CXDiagnosticDisplayOptions")})) |>
    add_function (Pcre.regexp "^clang_defaultDiagnosticDisplayOptions$")
      (empty_function_interface |>
        add_result (empty_type_interface |> reinterpret_as (Set_of "CXDiagnosticDisplayOptions"))) |>
    add_function (Pcre.regexp "^clang_defaultEditingTranslationUnitOptions$")
      (empty_function_interface |>
        add_result (empty_type_interface |> reinterpret_as (Set_of "CXTranslationUnit_Flags"))) |>
    add_function (Pcre.regexp "^clang_parseTranslationUnit")
      (empty_function_interface |>
        add_argument (Type_interface {argument = Name "options"; interface = empty_type_interface |> reinterpret_as (Set_of "CXTranslationUnit_Flags")})) |>
    add_function (Pcre.regexp "^clang_saveTranslationUnit$")
      (empty_function_interface |>
        add_argument (Type_interface {argument = Name "options"; interface = empty_type_interface |> reinterpret_as (Set_of "CXSaveTranslationUnit_Flags")})) |>
    add_function (Pcre.regexp "^clang_reparseTranslationUnit$")
      (empty_function_interface |>
        add_argument (Type_interface {argument = Name "options"; interface = empty_type_interface |> reinterpret_as (Set_of "CXReparse_Flags")})) |>
    add_function (Pcre.regexp "^clang_defaultReparseOptions$")
      (empty_function_interface |>
        add_result (empty_type_interface |> reinterpret_as (Set_of "CXReparse_Flags"))) |>
    add_function (Pcre.regexp "^(clang_(re)?parseTranslationUnit|\
                    clang_createTranslationUnitFromSourceFile$|\
                    clang_codeCompleteAt$|clang_indexSourceFile)")
      (empty_function_interface |>
        add_argument (Array {
          length = Argument (Name "num_unsaved_files");
          contents = Name "unsaved_files" })) |>
    add_function (Pcre.regexp "^clang_parseTranslationUnit2|^clang_createTranslationUnit2$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "out_TU"))) |>
    add_function (Pcre.regexp "^clang_visitChildren$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_zero_is_true) |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 2;
          references = [Name "parent"] })) |>
    add_function (Pcre.regexp "^clang_ext_DeclContext_visitDecls$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_zero_is_true) |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 2;
          references = [Name "parent"] })) |>
    add_function (Pcre.regexp "^clang_getInclusions$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean) |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 3;
          references = [Name "parent"] })) |>
    add_function (Pcre.regexp "^clang_Type_visitFields$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean) |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 1;
          references = [Name "T"]})) |>
    add_function (Pcre.regexp "^clang.*_(is|equal)[A-Z_]")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean) |>
        dont_label_unique) |>
    add_function (Pcre.regexp "^clang_compare_")
      (empty_function_interface |>
        dont_label_unique) |>
    add_enum (Pcre.regexp "^CXErrorCode$")
      (empty_enum_interface |>
        add_constant (Pcre.regexp "^CXError_Success$") (empty_constant_interface |> set_success)) |>
    add_enum (Pcre.regexp "^CXSaveError$")
      (empty_enum_interface |>
        add_constant (Pcre.regexp "^CXSaveError_None$") (empty_constant_interface |> set_success)) |>
    add_struct (Pcre.regexp "^CXUnsavedFile$")
      (empty_struct_interface |>
        add_field (Sized_string {length = "Length"; contents = "Contents"})) |>
    add_struct (Pcre.regexp "^CXType$")
      (empty_struct_interface |>
        add_accessor "kind" "get_type_kind" "clang_getTypeKind_wrapper") |>
    add_function (Pcre.regexp "^clang_saveTranslationUnit")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_enum "CXSaveError")) |>
    add_function (Pcre.regexp "^clang_reparseTranslationUnit")
      (empty_function_interface |>
        add_result (empty_type_interface |> result_cxerrorcode)) |>
    add_function (Pcre.regexp "^clang_getFileUniqueID$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_zero_is_true) |>
        add_argument (output_on_success (Name "outID"))) |>
    add_function (Pcre.regexp "^clang_indexSourceFile")
      (empty_function_interface |>
        add_result (empty_type_interface |> result_cxerrorcode) |>
        add_argument (output_on_success (Name "out_TU"))) |>
    add_function (Pcre.regexp "^clang_((indexLoc_)?getFile|getExpansion|getInstantiation|getSpelling)Location$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "file")) |>
        add_argument (output_on_success (Name "line")) |>
        add_argument (output_on_success (Name "column")) |>
        add_argument (output_on_success (Name "offset"))) |>
    add_function (Pcre.regexp "^clang_getPresumedLocation$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "filename")) |>
        add_argument (output_on_success (Name "line")) |>
        add_argument (output_on_success (Name "column"))) |>
    add_function (Pcre.regexp "^clang_indexLoc_getFileLocation$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "indexFile"))) |>
    add_function (Pcre.regexp "^clang_(ModuleMapDescriptor|VirtualFileOverlay)_writeToBuffer$")
      (empty_function_interface |>
        add_argument (Sized_string {length = Argument (Name "out_buffer_size"); contents = Name "out_buffer_ptr"}) |>
        add_argument (Type_interface {argument = Name "out_buffer_ptr"; interface = empty_type_interface |> make_destructor clang_free }) |>
        add_argument (output_on_success (Name "out_buffer_ptr"))) |>
    add_function (Pcre.regexp "^clang_getFileContents$")
      (empty_function_interface |>
        add_result (empty_type_interface |>
          reinterpret_as (Sized_string { length = Name "size" }) |>
          null_is_none)) |>
    add_function (Pcre.regexp "^clang_getDiagnosticOption$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "Disable"))) |>
    add_function (Pcre.regexp "^clang_Cursor_isExternalSymbol$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean) |>
        add_argument (output_on_success (Name "language")) |>
        add_argument (output_on_success (Name "definedIn")) |>
        add_argument (output_on_success (Name "isGenerated"))) |>
    add_function (Pcre.regexp "^clang_getCompletionParent$")
      (empty_function_interface |>
        add_argument (Fixed_value { argument = Name "kind"; value = "NULL" })) |>
    add_function (Pcre.regexp "^clang_getRemappingsFromFileList$")
      (empty_function_interface |>
        add_argument (Array { contents = Name "filePaths"; length = Argument (Name "numFiles") })) |>
    add_function (Pcre.regexp "^clang_getOverriddenCursors$")
      (empty_function_interface |>
        add_argument (Array { contents = Name "overridden"; length = Argument (Name "num_overridden") }) |>
        add_argument (Type_interface {argument = Name "overridden"; interface = empty_type_interface |> make_destructor "clang_disposeOverriddenCursors"}) |>
        add_argument (output_on_success (Name "overridden"))) |>
    add_function (Pcre.regexp "^clang_get(All)?SkippedRanges$")
      (empty_function_interface |>
        add_result (empty_type_interface |>
          reinterpret_as (Array_struct { length = "count"; contents = "ranges" }) |>
          make_destructor "clang_disposeSourceRangeList")) |>
    add_function (Pcre.regexp "^clang_tokenize$")
      (empty_function_interface |>
        add_argument (Array { contents = Name "Tokens"; length = Argument (Name "NumTokens") }) |>
        (*add_argument (Type_interface {argument = Name "Tokens"; interface = empty_type_interface |> destructor (fun s -> Printf.sprintf "clang_disposeTokens(TU, %s, NumTokens);" s)}) |>*)
        add_argument (output_on_success (Name "Tokens"))) |>
    add_function (Pcre.regexp "^clang_getDiagnosticFixIt$")
      (empty_function_interface |>
        add_argument (Update (Name "ReplacementRange"))) |>
    add_function (Pcre.regexp "^clang_getFileTime$")
      (empty_function_interface |>
        add_result (empty_type_interface |> reinterpret_as Int)) |>
    add_enum (Pcre.regexp "^CXLoadDiag_Error$")
      (empty_enum_interface |>
        add_constant (Pcre.regexp "^CXLoadDiag_None$") (empty_constant_interface |> set_success)) |>
    add_function (Pcre.regexp "^clang_loadDiagnostics$")
      (empty_function_interface |>
        add_argument (output (Name "error")) |>
        add_argument (output_on_error (Name "errorString"))) |>
    add_function (Pcre.regexp "^clang_createIndex$")
      (empty_function_interface |>
        add_argument (Type_interface { argument = Name "excludeDeclarationsFromPCH"; interface = empty_type_interface |> integer_boolean }) |>
        add_argument (Type_interface { argument = Name "displayDiagnostics"; interface = empty_type_interface |> integer_boolean })) |>
    add_function (Pcre.regexp "^clang_getRange$")
      (empty_function_interface |>
        dont_label_unique) |>
    add_function (Pcre.regexp "^clang_getCursorPlatformAvailability$")
      (empty_function_interface |>
        add_argument (Type_interface { argument = Name "always_deprecated"; interface = empty_type_interface |> integer_boolean }) |>
        add_argument (output (Name "always_deprecated")) |>
        add_argument (output (Name "deprecated_message")) |>
        add_argument (Type_interface { argument = Name "always_unavailable"; interface = empty_type_interface |> integer_boolean }) |>
        add_argument (output (Name "always_unavailable")) |>
        add_argument (output (Name "unavailable_message"))) |>
    add_function (Pcre.regexp "^clang_Cursor_get(CXX|ObjC)Manglings$")
      (empty_function_interface |>
        add_result (empty_type_interface |>
          reinterpret_as (Array_struct { length = "Count"; contents = "Strings" }) |>
          make_destructor "clang_disposeStringSet")) |>
    add_function (Pcre.regexp "^clang_Cursor_getBriefCommentText$")
      (empty_function_interface |>
        add_result (empty_type_interface |>
          reinterpret_as string_option)) |>
    add_function (Pcre.regexp "^clang_ext_LinkageSpecDecl_getLanguageIDs$")
      (empty_function_interface |>
        add_result (empty_type_interface |>
          reinterpret_as (Set_of "clang_ext_LanguageIDs"))) in
  let idx = Clang.create_index true true in
  Format.printf "%a@." (Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
      Format.pp_print_string)
    clang_options;
  let tu =
    match
      Clang.parse_translation_unit2 idx
        "source.c"
        (Array.of_list clang_options)
        [| { filename = "source.c"; contents = "\
#include <clang-c/Index.h>
#include \"clangml/clang__custom.h\"
#include \"clangml/libclang_extensions.h\"" } |]
        Clang.Cxtranslationunit_flags.none with
    | Error _ -> failwith "Error!"
    | Ok tu -> tu in
  if Clang.has_severity Clang.error tu then
    failwith "Clang compilation error";
  let cur = Clang.get_translation_unit_cursor tu in
  let chan_stubs = open_out (prefix ^ "clang_stubs.c") in
  Fun.protect ~finally:(fun () -> close_out chan_stubs) (fun () ->
    Stubgen_common.output_warning_c chan_stubs tool_name;
    output_string chan_stubs "\
#include \"stubgen.h\"
#include <clang-c/Index.h>
#include \"clang__custom.h\"
#include \"libclang_extensions.h\"
";
    let context =
      create_translation_context module_interface chan_stubs in
    ignore (Clang.visit_children cur (fun cur par ->
      begin
        match Clang.get_cursor_kind cur with
        | StructDecl ->
            translate_struct_decl context cur
        | EnumDecl ->
            translate_enum_decl context cur
        | FunctionDecl ->
            translate_function_decl context cur
        | TypedefDecl ->
            translate_typedef_decl context cur
        | FieldDecl
        | EnumConstantDecl -> assert false
        | _ -> ()
      end;
      Continue));
    let chan_intf = open_out (prefix ^ "clang__bindings.mli") in
    Fun.protect ~finally:(fun () -> close_out chan_intf) (fun () ->
      Stubgen_common.output_warning_ml chan_intf tool_name;
      Format.fprintf (Format.formatter_of_out_channel chan_intf)
        "%a@." Ppxlib.Pprintast.signature (List.rev context.sig_accu));
    let chan_impl = open_out (prefix ^ "clang__bindings.ml") in
    Fun.protect ~finally:(fun () -> close_out chan_intf) (fun () ->
      Stubgen_common.output_warning_ml chan_impl tool_name;
      Format.fprintf (Format.formatter_of_out_channel chan_impl)
        "%a@." Ppxlib.Pprintast.structure (List.rev context.struct_accu)))

let info =
  let doc = "generate stubs for ClangML" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info tool_name ~doc ~exits:Cmdliner.Term.default_exits ~man

let () =
  Cmdliner.Term.exit (Cmdliner.Term.eval (Stubgen_common.options main, info))
