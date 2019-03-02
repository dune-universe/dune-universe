open Base
open Ppxlib
open Ast_builder.Default

let extension_name = "xml"

let unsupported_type_error_msg ~name =
  Printf.sprintf "The type %s is not natively supported in the xml camlp4 extension" name

let useless_merge_recursive _log ~field_name:_ ~tp:_ ast = ast

let xsd_function_name = function
  | None -> "xsd"
  | Some param -> Printf.sprintf "xsd_of_%s" param

let edot ~loc path_opt id =
  pexp_ident ~loc
    (Located.mk ~loc
     (match path_opt with
      | None   -> Longident.Lident id
      | Some p -> Longident.Ldot (p, id)))

module Xsd_of_type = Ppx_conv_func.Of_complete (struct
  let unsupported_type_error_msg = unsupported_type_error_msg
  let conversion_name = extension_name
  let function_name = xsd_function_name

  let prepend loc body =
    let acc loc = [%pat?  acc ] in
    let anonymous loc = [%pat?  _ ] in
    let body = [%expr [%e body] :: acc ] in
    Ppx_conv_func.lambda loc [ acc loc; anonymous loc] body

  let unit loc ~field_name:_ =
    let expr = [%expr  Csvfields.Xml.xsd_of_unit ] in
    prepend loc expr

  let bool loc ~field_name:_      = [%expr  Csvfields.Xml.xsd_of_bool ]
  let string loc ~field_name:_    = [%expr  Csvfields.Xml.xsd_of_string ]
  let char loc ~field_name:_      = [%expr  Csvfields.Xml.xsd_of_char ]
  let int loc ~field_name:_       = [%expr  Csvfields.Xml.xsd_of_int ]
  let float loc ~field_name:_     = [%expr  Csvfields.Xml.xsd_of_float ]
  let int32 loc ~field_name:_     = [%expr  Csvfields.Xml.xsd_of_int32 ]
  let int64 loc ~field_name:_     = [%expr  Csvfields.Xml.xsd_of_int64 ]
  let nativeint loc ~field_name:_ = [%expr  Csvfields.Xml.xsd_of_nativeint ]
  let big_int loc ~field_name:_   = [%expr  Csvfields.Xml.xsd_of_big_int ]
  let nat loc ~field_name:_       = [%expr  Csvfields.Xml.xsd_of_nat ]
  let num loc ~field_name:_       = [%expr  Csvfields.Xml.xsd_of_num ]
  let ratio loc ~field_name:_     = [%expr  Csvfields.Xml.xsd_of_ratio ]

  let merge_recursive = useless_merge_recursive

  let recursive loc ~field_name:_ ~type_name:_ ~path =
    let tns = "xsd" in
    edot ~loc path tns

  let list loc ~field_name =
    [%expr  Csvfields.Xml.xsd_of_list [%e estring ~loc field_name] ]

  let array loc ~field_name =
    [%expr  Csvfields.Xml.xsd_of_array [%e estring ~loc field_name] ]

  let option loc ~field_name =
    [%expr  Csvfields.Xml.xsd_of_option [%e estring ~loc field_name] ]

  let lazy_t loc ~field_name:_ =
    [%expr  Csvfields.Xml.xsd_of_lazy_t ]

  let ref loc ~field_name:_ =
    [%expr  Csvfields.Xml.xsd_of_ref ]

end)

let type_of_xml_function_name = function
    | None -> "of_xml"
    | Some param -> Printf.sprintf  "%s_of_xml" param

module Type_of_xml = Ppx_conv_func.Of_complete (struct
  let unsupported_type_error_msg = unsupported_type_error_msg
  let conversion_name = extension_name
  let function_name = type_of_xml_function_name

  let unit loc ~field_name:_ =
    [%expr  Csvfields.Xml.unit_of_xml ]
  let bool loc ~field_name:_ =
    [%expr  Csvfields.Xml.bool_of_xml ]
  let string loc ~field_name:_ =
    [%expr  Csvfields.Xml.string_of_xml ]
  let char loc ~field_name:_ =
    [%expr  Csvfields.Xml.char_of_xml ]
  let int loc ~field_name:_ =
    [%expr  Csvfields.Xml.int_of_xml ]
  let float loc ~field_name:_ =
    [%expr  Csvfields.Xml.float_of_xml ]
  let int32 loc ~field_name:_ =
    [%expr  Csvfields.Xml.int32_of_xml ]
  let int64 loc ~field_name:_ =
    [%expr  Csvfields.Xml.int64_of_xml ]
  let nativeint loc ~field_name:_ =
    [%expr  Csvfields.Xml.nativeint_of_xml ]
  let big_int loc ~field_name:_ =
    [%expr  Csvfields.Xml.big_int_of_xml ]
  let nat loc ~field_name:_ =
    [%expr  Csvfields.Xml.nat_of_xml ]
  let num loc ~field_name:_ =
    [%expr  Csvfields.Xml.num_of_xml ]
  let ratio loc ~field_name:_ =
    [%expr  Csvfields.Xml.ratio_of_xml ]

  let merge_recursive loc ~field_name ~tp expr =
    match tp.ptyp_desc with
    | Ptyp_constr (id, [_]) ->
        (match id.txt with
        | Lident "sexp_option"
        | Lident "option" ->
          [%expr
            (fun xml ->
               match Csvfields.Xml.child xml [%e estring ~loc field_name] with
               | None -> None
               | Some _xml -> ([%e expr] xml)
            ) ]
        | _ -> expr)
    | _ -> expr

  let recursive loc ~field_name ~type_name ~path =
    let tns = "of_xml" in
    match type_name with
    | "sexp_option" | "option" | "list" | "array" ->
      [%expr [%e edot ~loc path tns] ~tag:[%e estring ~loc field_name] ]
    | _ ->
      edot ~loc path tns

  let list loc ~field_name =
    [%expr  Csvfields.Xml.list_of_xml ~tag:[%e estring ~loc field_name] ]

  let array loc ~field_name =
    [%expr  Csvfields.Xml.array_of_xml ~tag:[%e estring ~loc field_name] ]

  let option loc ~field_name =
    [%expr  Csvfields.Xml.option_of_xml ~tag:[%e estring ~loc field_name] ]

  let lazy_t loc ~field_name:_ =
    [%expr  Csvfields.Xml.lazy_t_of_xml ]

  let ref loc ~field_name:_ =
    [%expr  Csvfields.Xml.ref_of_xml ]

end)

let xml_of_type_function_name = function
    | None -> "to_xml"
    | Some param -> Printf.sprintf "%s_to_xml" param

module Xml_of_type = Ppx_conv_func.Of_complete (struct
  let unsupported_type_error_msg = unsupported_type_error_msg
  let conversion_name = extension_name
  let function_name = xml_of_type_function_name

  let unit loc ~field_name:_   = [%expr  Csvfields.Xml.xml_of_unit ]
  let bool loc ~field_name:_   = [%expr  Csvfields.Xml.xml_of_bool ]
  let string loc ~field_name:_ = [%expr  Csvfields.Xml.xml_of_string ]
  let char loc ~field_name:_   = [%expr  Csvfields.Xml.xml_of_char ]
  let int loc ~field_name:_    = [%expr  Csvfields.Xml.xml_of_int ]
  let float loc ~field_name:_  = [%expr  Csvfields.Xml.xml_of_float ]
  let int32 loc ~field_name:_  = [%expr  Csvfields.Xml.xml_of_int32 ]
  let int64 loc ~field_name:_  = [%expr  Csvfields.Xml.xml_of_int64 ]
  let big_int loc ~field_name:_ = [%expr  Csvfields.Xml.xml_of_big_int ]
  let nat loc ~field_name:_     = [%expr  Csvfields.Xml.xml_of_nat ]
  let num loc ~field_name:_     = [%expr  Csvfields.Xml.xml_of_num ]
  let ratio loc ~field_name:_   = [%expr  Csvfields.Xml.xml_of_ratio ]
  let nativeint loc ~field_name:_ =
    [%expr  Csvfields.Xml.xml_of_nativeint ]

  let merge_recursive = useless_merge_recursive

  let recursive loc ~field_name ~type_name ~path =
    let tns = function_name None in
    match type_name with
    | "sexp_option" | "option" | "list" | "array" ->
      [%expr  [%e edot ~loc path tns] ~tag:[%e estring ~loc field_name] ]
    | _ ->
      edot ~loc path tns

  let list loc ~field_name =
    [%expr  Csvfields.Xml.xml_of_list ~tag:[%e estring ~loc field_name] ]

  let array loc ~field_name =
    [%expr  Csvfields.Xml.xml_of_array ~tag:[%e estring ~loc field_name] ]

  let option loc ~field_name =
    [%expr  (Csvfields.Xml.xml_of_option ~tag:[%e estring ~loc field_name]) ]

  let lazy_t loc ~field_name:_ =
    [%expr  Csvfields.Xml.xml_of_lazy_t ]

  let ref loc ~field_name:_ =
    [%expr  Csvfields.Xml.xml_of_ref ]

end)

let make_xsd_definition loc arg_tys =
  List.fold_right arg_tys ~init:[%type: Csvfields.Xml.xml list ]
    ~f:(fun _arg_ty acc ->
      [%type: Csvfields.Xml.xml list -> [%t acc] ])

let xml_record_sig ~tps ~record_name loc =
  let xsd = make_xsd_definition loc tps in
  let record_type = ptyp_constr ~loc (Located.lident ~loc record_name) tps in
  let to_xml =
    let base = [%type: Csvfields.Xml.xml list] in
    let tps = List.map tps ~f:(fun arg_ty ->
      [%type: ([%t arg_ty] -> [%t base]) ] )
    in
    let init = [%type: [%t record_type] -> Csvfields.Xml.xml list] in
    List.fold_right tps ~init ~f:(fun arg acc -> [%type: [%t arg] -> [%t acc] ])
  in
  let of_xml =
    let base = [%type:  Csvfields.Xml.xml ] in
    let tps = List.map tps ~f:(fun arg_ty ->
      [%type: ([%t base] -> [%t arg_ty]) ] )
    in
    let init = [%type:  Csvfields.Xml.xml -> [%t record_type] ] in
    List.fold_right tps ~init ~f:(fun arg acc -> [%type: [%t arg] -> [%t acc] ])
  in
  [ psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc "xsd") ~type_:xsd ~prim:[])
  ; psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc "to_xml") ~type_:to_xml ~prim:[])
  ; psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc "of_xml") ~type_:of_xml ~prim:[])
  ]

let param_of_ctyp ty =
  match ty.ptyp_desc with
  | Ptyp_var param -> Some param
  | _ -> None

let make_parameter_functions loc ~function_name tps =
  List.fold_right tps ~init:[]
    ~f:(fun name acc ->
      let name = function_name (param_of_ctyp name) in
      pvar ~loc name :: acc)

let is_list_array_or_option field_ty =
  match field_ty.ptyp_desc with
  | Ptyp_constr (id, [_]) ->
    (match id.txt with
    | Lident ( "sexp_option"
             | "option"
             | "list"
             | "array") -> true
    | _ -> false)
  | _ -> false

let of_xml ~tps ~lds loc =
  let fields = Ppx_conv_func.Gen_struct.fields lds in
  let bindings =
    List.map ~f:(fun (field_name, _, field_ty) ->
      let conversion_fun =
        Type_of_xml.conversion_of_type loc ~field_name ~field_ty
      in
      let conversion_fun = [%expr [%e conversion_fun]] in
      if is_list_array_or_option field_ty then
        (Located.lident ~loc field_name, [%expr [%e conversion_fun] xml])
      else
        (Located.lident ~loc field_name,
         [%expr
           Csvfields.Xml.recursive_of_xml [%e estring ~loc field_name] [%e conversion_fun]
             xml
         ]))
    fields
  in
  let record = pexp_record ~loc bindings None in
  let arguments =
    (make_parameter_functions loc ~function_name:type_of_xml_function_name tps)
    @ [ [%pat?  xml ] ]
  in
  let labels = List.fold_right ~f:(fun (x,_,_) acc -> [%expr [%e estring ~loc x] :: [%e acc] ])
    fields ~init:[%expr  [] ]
  in
  let check = [%expr  Csvfields.Xml.check_extra_fields xml [%e labels] ] in
  let body = Ppx_conv_func.lambda loc arguments (pexp_sequence ~loc check record) in
  [%stri let of_xml = [%e body] ]

let prepend loc body =
  let acc = [%pat?  acc ] in
  let field = [%pat?  _field ] in
  let body = [%expr [%e body] :: acc ] in
  Ppx_conv_func.lambda loc [ acc; field ] body

let prepend_at loc body =
  let acc = [%pat?  acc ] in
  let field = [%pat?  _field ] in
  let body = [%expr   [%e body] @ acc  ] in
  Ppx_conv_func.lambda loc [ acc; field ] body

let xsd ~tps ~lds loc =
  let name = [%pat?  xsd ] in
  let conversion_of_type loc ~field_name ~field_ty =
    let body = Xsd_of_type.conversion_of_type loc ~field_name ~field_ty in
    prepend loc
      (if is_list_array_or_option field_ty then
        [%expr  Csvfields.Xml.decomplexify [%e body] ]
      else
        [%expr  Csvfields.Xml.xsd_element ~name:[%e estring ~loc field_name] [%e body] ] )
  in
  let wrap_body body =
    let tps = make_parameter_functions loc ~function_name:xsd_function_name tps in
    Ppx_conv_func.lambda loc tps [%expr [ Csvfields.Xml.complex_type [%e body] ] ]
  in
  Ppx_conv_func.Gen_struct.generate_using_fold
    ~wrap_body ~pass_acc:false ~pass_anonymous:false ~conversion_of_type ~name ~lds loc

let to_xml ~tps ~lds loc =
  let name = [%pat?  to_xml ] in
  let conversion_of_type loc ~field_name ~field_ty =
    let body = Xml_of_type.conversion_of_type loc ~field_name ~field_ty in
    let expr =
      if is_list_array_or_option field_ty then
        [%expr  ([%e body] (Fieldslib.Field.get _field t)) ]
      else
        [%expr  [
          Csvfields.Xml.create_node
            ~tag:[%e estring ~loc field_name] ~body:([%e body] (Fieldslib.Field.get _field t))] ]
    in
    prepend_at loc expr
  in
  let wrap_body body =
    let tps =
      make_parameter_functions loc ~function_name:xml_of_type_function_name tps
    in
    let arguments = tps @ [ [%pat?  t ] ]in
    Ppx_conv_func.lambda loc arguments body
  in
  Ppx_conv_func.Gen_struct.generate_using_fold
    ~wrap_body ~pass_acc:false ~pass_anonymous:false
    ~conversion_of_type ~name ~lds loc

let xml_record ~tps ~record_name:_ loc lds =
  let xsd = xsd ~tps ~lds loc in
  let to_xml = to_xml ~tps ~lds loc in
  let of_xml = of_xml ~tps ~lds loc in
  [ to_xml
  ; of_xml
  ; xsd
  ]

let raise_unsupported ~loc s =
  Location.raise_errorf ~loc
    "Unsupported use of %s \
     (you can only use it on records, abstract types or type aliases)." s

let xml_sig_of_ty ~extension_name =
  Ppx_conv_func.Gen_sig.generate
    ~extension_name
    ~nil:xml_record_sig
    ~record:(fun ~tps ~record_name loc _ -> xml_record_sig ~tps ~record_name loc)

let generate_sig ~extension_name ~loc ~path ((_rf, tds) as x) =
  match tds with
  | [_] -> xml_sig_of_ty ~extension_name ~loc ~path x
  | _   -> raise_unsupported ~loc extension_name

let xml =
  Deriving.add extension_name
    ~sig_type_decl:(Deriving.Generator.make_noarg (generate_sig ~extension_name))
    ~str_type_decl:(Deriving.Generator.make_noarg
                      (Ppx_conv_func.Gen_struct.generate
                         ~extension_name
                         ~record:xml_record)
                      ~deps:[Ppx_fields_conv.fields])
;;
