open Ppxlib

module Attr = struct
  let value =
    Attribute.declare
      "ppx_enum.enum.value"
      Attribute.Context.constructor_declaration
      Ast_pattern.(single_expr_payload (estring __))
      (fun x -> x)

  let packed = [Attribute.T value]
end

module Str = struct
  let string_to_constant_expression ~loc ~str =
    Ast_builder.Default.pexp_constant
      ~loc
      (Pconst_string (str, None))

  let string_to_constant_pattern ~loc ~str =
    Ast_builder.Default.ppat_constant
      ~loc
      (Pconst_string (str, None))

  let string_to_constructor_pattern ~loc ~str =
    Ast_builder.Default.ppat_construct
      ~loc
      {txt = Lident str; loc}
      None

  let string_to_constructor_expression ~loc ~str =
    Ast_builder.Default.pexp_construct
      ~loc
      {txt = Lident str; loc}
      None

  let constructor_name_and_value ({pcd_name = {txt = name; _}; _} as constructor) =
    let attribute_value = Attribute.get Attr.value constructor in
    let value =
      match attribute_value with
      | Some value -> value
      | None -> name
    in
    (name, value)

  let to_string_case_from_name_and_value ~loc (name, value) =
    let lhs = string_to_constructor_pattern ~loc ~str:name in
    let rhs = string_to_constant_expression ~loc ~str:value in
    Ast_builder.Default.case ~lhs ~guard:None ~rhs

  let assert_no_duplicate_values ~loc constructor_details =
    let unique_values = List.sort_uniq String.compare @@ snd @@ List.split constructor_details in
    if List.compare_lengths constructor_details unique_values != 0 then
        Raise.Enum.errorf ~loc "cannot derive enum. Enums must have unique values"

  let to_string_constructor_cases ~loc constructors =
    let constructor_details = List.map constructor_name_and_value constructors in
    assert_no_duplicate_values ~loc constructor_details;
    List.map (to_string_case_from_name_and_value ~loc) constructor_details

  let to_string_function ~loc ~type_name ~constructors =
    let function_name = Utils.to_string_function_name ~enum_name:type_name in
    let pat = Ast_builder.Default.ppat_var ~loc {txt=function_name; loc} in
    let cases =to_string_constructor_cases ~loc constructors in
    let expr =
      Ast_builder.Default.pexp_function
        ~loc
        cases
    in
    let value_description =
      Ast_builder.Default.value_binding
      ~loc
      ~pat
      ~expr
    in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_description]

  let from_string_case_from_name_and_value ~loc ~raises (name, value) =
    let lhs = string_to_constant_pattern ~loc ~str:value in
    let value_t = string_to_constructor_expression ~loc ~str:name in
    let rhs =
      if raises then
        value_t
      else
        [%expr Ok [%e value_t]]
    in
    Ast_builder.Default.case ~lhs ~guard:None ~rhs

  let from_string_constructor_cases ~loc ~raises constructors =
    constructors
    |> List.map constructor_name_and_value
    |> List.map (from_string_case_from_name_and_value ~loc ~raises)

  let invalid_case_for_from_string ~loc ~raises ~function_name =
    let lhs = [%pat? s] in
    let error_message =
      [%expr
        Printf.sprintf
          "Unexpected value for %s.%s: %s"
          __MODULE__
          [%e string_to_constant_expression ~loc ~str:function_name]
          s
      ]
    in
    let rhs =
      if raises then
        [%expr invalid_arg [%e error_message]]
      else
        [%expr Error [%e error_message]]
    in
    Ast_builder.Default.case ~lhs ~guard:None ~rhs

  let from_string_function_base ~loc ~raises ~function_name ~constructors =
    let pat = Ast_builder.Default.ppat_var ~loc {txt=function_name; loc} in
    let cases = from_string_constructor_cases ~loc ~raises constructors in
    let cases = cases @ [invalid_case_for_from_string ~loc ~raises ~function_name] in
    let expr = Ast_builder.Default.pexp_function ~loc cases in
    let value_description =
      Ast_builder.Default.value_binding
      ~loc
      ~pat
      ~expr
    in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_description]

  let from_string_function ~type_name =
    let function_name = Utils.from_string_function_name ~enum_name:type_name in
    from_string_function_base ~raises:false ~function_name

  let from_string_exn_function ~type_name =
    let function_name = Utils.from_string_exn_function_name ~enum_name:type_name in
    from_string_function_base ~raises:true ~function_name

  let from_enummable_variant
    ~loc
    ~type_name
    ~constructors
  =
    [ to_string_function ~loc ~type_name ~constructors
    ; from_string_function ~loc ~type_name ~constructors
    ; from_string_exn_function ~loc ~type_name ~constructors
    ]

  let from_type_declaration ~loc type_ =
    match type_ with
    | { ptype_kind = Ptype_variant constructors
      ; ptype_params = []
      ; ptype_name = {txt = type_name; _}
      ; ptype_loc
      ; _
      }
      when (Utils.constructors_are_bare constructors)
      ->
        from_enummable_variant ~loc:ptype_loc ~type_name ~constructors
    | {ptype_kind = Ptype_variant _; ptype_params = []; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "variant with arguments"
    | {ptype_kind = Ptype_variant _; ptype_params = _::_; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "parametrized variant"
    | {ptype_kind = Ptype_record _; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "record"
    | {ptype_kind = Ptype_abstract; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "abstract"
    | {ptype_kind = Ptype_open; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "open"

  (** By giving this to the Deriving.Generator.make_noarg function below, ppxlib
   *  will apply the function the parameters:
   *  ~loc: Information about the current location in the code base (file, lineno etc)
   *  ~path: The current file path?
   *  rec_flag: ???
   *  type_declarations: A list of the type declarations at the point of call
   *)
  let from_type_decl ~loc ~path:_ (_rec_flag, type_declarations) =
    List.flatten @@ List.map (from_type_declaration ~loc) type_declarations
end

module Sig = struct
  let to_string_function_val ~loc ~type_name =
    let function_name = Utils.to_string_function_name ~enum_name:type_name in
    let type_lident = {txt = Lident type_name; loc} in
    let lhs_type = Ast_builder.Default.ptyp_constr ~loc type_lident [] in
    let type_ = [%type: [%t lhs_type] -> string] in
    let value_description =
      Ast_builder.Default.value_description
      ~loc
      ~name:{txt = function_name; loc}
      ~type_
      ~prim:[]
    in
    Ast_builder.Default.psig_value ~loc value_description

  let from_string_function_val_base ~loc ~raises ~function_name ~type_name =
    let type_lident = {txt = Lident type_name; loc} in
    let type_t = Ast_builder.Default.ptyp_constr ~loc type_lident [] in
    let rhs_type =
      if raises
      then
        type_t
      else
        [%type: ([%t type_t], string) result]
    in
    let type_ = [%type: string -> [%t rhs_type]] in
    let value_description =
      Ast_builder.Default.value_description
      ~loc
      ~name:{txt = function_name; loc}
      ~type_
      ~prim:[]
    in
    Ast_builder.Default.psig_value ~loc value_description

  let from_string_function_val ~type_name =
    let function_name = Utils.from_string_function_name ~enum_name:type_name in
    from_string_function_val_base ~raises:false ~function_name ~type_name

  let from_string_exn_function_val ~type_name =
    let function_name = Utils.from_string_exn_function_name ~enum_name:type_name in
    from_string_function_val_base ~raises:true ~function_name ~type_name

  let assert_no_values_for_constructors ~loc constructors =
    let value_opts = List.map (Attribute.get Attr.value) constructors in
    let value_present =
      List.exists
        ( function
          | Some _ -> true
          | None -> false
        )
        value_opts
    in
    if value_present then
      Raise.Enum.errorf ~loc "custom enum values must not be declared in signatures."


  let from_enummable_variant ~loc ~type_name =
    [ to_string_function_val ~loc ~type_name
    ; from_string_function_val ~loc ~type_name
    ; from_string_exn_function_val ~loc ~type_name
    ]

  let from_type_declaration ~loc type_ =
    match type_ with
    | { ptype_kind = Ptype_variant constructors
      ; ptype_params = []
      ; ptype_name = {txt = type_name; _}
      ; ptype_loc
      ; _
      }
      when Utils.constructors_are_bare constructors
      ->
        assert_no_values_for_constructors ~loc:ptype_loc constructors;
        from_enummable_variant ~loc:ptype_loc ~type_name
    | {ptype_kind = Ptype_variant _; ptype_params = []; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "variant with arguments"
    | {ptype_kind = Ptype_variant _; ptype_params = _::_; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "parametrized variant"
    | {ptype_kind = Ptype_record _; _} -> Raise.Enum.unhandled_type_kind ~loc "record"
    | {ptype_kind = Ptype_abstract; _} -> Raise.Enum.unhandled_type_kind ~loc "abstract"
    | {ptype_kind = Ptype_open; _} -> Raise.Enum.unhandled_type_kind ~loc "open"

  let from_type_decl ~loc ~path:_ (_rec_flag, type_declarations) =
    List.flatten @@ List.map (from_type_declaration ~loc) type_declarations
end


let from_str_type_decl =
  Deriving.Generator.make_noarg
    ~attributes:Attr.packed
    Str.from_type_decl

let from_sig_type_decl =
  Deriving.Generator.make_noarg Sig.from_type_decl
