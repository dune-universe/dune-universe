open Base
module Protobuf = Shared.Protobuf
module Generated_code = Shared.Generated_code
module Module_name = Protobuf.Module_name
module Module_path = Protobuf.Module_path
module Variant_name = Protobuf.Variant_name
module Field_name = Protobuf.Field_name
module Enum = Protobuf.Enum
module Field = Protobuf.Field
module Message = Protobuf.Message
module File = Protobuf.File

type options = {derivers : string list}

module Code = struct
  type t =
    | Line of string
    | Block of block

  and block = {
    indented : bool;
    contents : t list;
  }

  type module_ = {
    module_name : Module_name.t;
    signature : t list;
    implementation : t list;
  }

  let line string = Line string

  let block ?(indented = true) contents = Block {indented; contents}

  let lines ?(indented = true) strings = strings |> List.map ~f:line |> block ~indented

  let make_list elements =
    block [line "["; elements |> List.map ~f:(fun e -> e ^ ";") |> lines; line "]"]

  let make_record ?(prefix = "") elements =
    line @@ Printf.sprintf "%s { %s }" prefix (String.concat elements ~sep:"; ")

  let make_record_or_unit ?(prefix = "") elements =
    match List.length elements with
    | 0 -> line @@ Printf.sprintf "%s ()" prefix
    | _ -> make_record ~prefix elements

  let with_deriving ~options:{derivers} type_code =
    let deriving =
      match derivers with
      | [] -> []
      | _ ->
          [String.concat ~sep:", " derivers |> Printf.sprintf "[@@deriving %s]" |> line]
    in
    [type_code; deriving] |> List.concat |> block

  let make_record_type ~options type_name fields =
    with_deriving
      ~options
      [
        type_name |> Printf.sprintf "type %s = {" |> line;
        fields
        |> List.map ~f:(fun (field_name, field_type) ->
               Printf.sprintf "%s : %s;" field_name field_type)
        |> lines;
        line "}";
      ]

  let make_unit_type ~options type_name =
    with_deriving ~options [type_name |> Printf.sprintf "type %s = unit" |> line]

  let make_record_type_or_unit ~options type_name elements =
    match List.length elements with
    | 0 -> make_unit_type ~options type_name
    | _ -> make_record_type ~options type_name elements

  let make_variant_type ~options:{derivers} type_name values =
    let deriving =
      match derivers with
      | [] -> []
      | _ ->
          [String.concat ~sep:", " derivers |> Printf.sprintf "[@@deriving %s]" |> line]
    in
    [
      [
        type_name |> Printf.sprintf "type %s =" |> line;
        values |> List.map ~f:(Printf.sprintf "| %s") |> lines;
      ];
      deriving;
    ]
    |> List.concat
    |> block

  let make_let ~recursive name code =
    let prefix =
      match recursive with
      | true -> "rec "
      | false -> ""
    in
    block [Printf.sprintf "let %s%s =" prefix name |> line; code]

  let make_match ~bracketed ~suffix expression cases =
    let prefix, suffix, pattern_prefix =
      match bracketed with
      | true -> "(", Printf.sprintf ")%s" suffix, " "
      | false -> "", suffix, ""
    in
    let match_line = Printf.sprintf "%smatch %s with" prefix expression in
    let case_lines =
      cases
      |> List.fold_right ~init:(true, []) ~f:(fun (pattern, code) (is_last, acc) ->
             let suffix = if is_last then suffix else "" in
             let case =
               Printf.sprintf "%s| %s -> %s%s" pattern_prefix pattern code suffix
             in
             false, case :: acc)
      |> snd
    in
    match_line :: case_lines |> lines ~indented:false

  let make_lambda argument body =
    block [argument |> Printf.sprintf "fun %s ->" |> line; body]

  let add_vertical_space items =
    items
    |> List.filter ~f:(function
           | Block {contents = []; _} -> false
           | _ -> true)
    |> List.intersperse ~sep:(line "")

  let make_modules ~recursive ~with_implementation (modules : module_ list) =
    let rec modules_code is_first acc = function
      | [] -> List.rev acc
      | {module_name; signature; implementation} :: rest ->
          let module_name = Module_name.to_string module_name in
          let prefix =
            match recursive, is_first with
            | false, true -> [Printf.sprintf "module %s : sig" module_name]
            | false, false -> [""; Printf.sprintf "module %s : sig" module_name]
            | true, true -> [Printf.sprintf "module rec %s : sig" module_name]
            | true, false -> [""; Printf.sprintf "and %s : sig" module_name]
          in
          let code =
            List.concat
              [
                List.map prefix ~f:line;
                add_vertical_space signature;
                (match with_implementation with
                | true ->
                    List.concat
                      [[line "end = struct"]; add_vertical_space implementation]
                | false -> []);
                [line "end"];
              ]
          in
          modules_code false (block ~indented:false code :: acc) rest
    in
    modules_code true [] modules

  let make_file items =
    let items = add_vertical_space items in
    block ~indented:false items

  let emit code =
    let rec append ~indent buffer = function
      | Line string -> List.iter [indent; string; "\n"] ~f:(Buffer.add_string buffer)
      | Block {indented; contents} ->
          let indent = if indented then Printf.sprintf "  %s" indent else indent in
          List.iter contents ~f:(fun each -> append buffer ~indent each)
    in
    let buffer = Base.Buffer.create 4096 in
    append buffer ~indent:"" code; Buffer.contents buffer
end

let generate_enum : options:options -> Enum.t -> Code.module_ =
 fun ~options {module_name; values; _} ->
  let type_declaration =
    values
    |> List.map ~f:(fun Enum.{variant_name; _} -> variant_name)
    |> List.map ~f:Variant_name.to_string
    |> Code.make_variant_type ~options "t"
  in
  let signature =
    Code.
      [
        type_declaration;
        block [line "val default : unit -> t"];
        block [line "val to_int : t -> int"];
        block [line "val of_int : int -> t option"];
        block [line "val to_string : t -> string"];
        block [line "val of_string : string -> t option"];
      ]
  in
  let unique_values =
    values
    |> List.stable_sort ~compare:(fun Enum.{id = id1; _} {id = id2; _} -> id1 - id2)
    |> List.group ~break:(fun Enum.{id = id1; _} {id = id2; _} -> id1 <> id2)
    |> List.map ~f:List.hd_exn
  in
  let implementation =
    let default_value =
      List.find values ~f:(fun {id; _} -> id = 0)
      |> Option.value ~default:(List.hd_exn values)
    in
    let default_function =
      [
        default_value.variant_name
        |> Variant_name.to_string
        |> Printf.sprintf "fun () -> %s"
        |> Code.line;
      ]
    in
    let to_int_function =
      List.concat
        Code.
          [
            [line "function"];
            values
            |> List.map ~f:(fun Enum.{id; variant_name; _} ->
                   let variant_name = Variant_name.to_string variant_name in
                   line (Printf.sprintf "| %s -> %d" variant_name id));
          ]
    in
    let of_int_function =
      List.concat
        Code.
          [
            [line "function"];
            unique_values
            |> List.map ~f:(fun Enum.{id; variant_name; _} ->
                   let variant_name = Variant_name.to_string variant_name in
                   line (Printf.sprintf "| %d -> Some %s" id variant_name));
            [line "| _ -> None"];
          ]
    in
    let to_string_function =
      List.concat
        Code.
          [
            [line "function"];
            values
            |> List.map ~f:(fun Enum.{variant_name; original_name; _} ->
                   let variant_name = Variant_name.to_string variant_name in
                   line (Printf.sprintf {|| %s -> "%s"|} variant_name original_name));
          ]
    in
    let of_string_function =
      List.concat
        Code.
          [
            [line "function"];
            values
            |> List.map ~f:(fun Enum.{variant_name; original_name; _} ->
                   let variant_name = Variant_name.to_string variant_name in
                   line (Printf.sprintf {|| "%s" -> Some %s|} original_name variant_name));
            [line "| _ -> None"];
          ]
    in
    Code.
      [
        type_declaration;
        make_let ~recursive:false "default" @@ block default_function;
        make_let ~recursive:false "to_int" @@ block to_int_function;
        make_let ~recursive:false "of_int" @@ block of_int_function;
        make_let ~recursive:false "to_string" @@ block to_string_function;
        make_let ~recursive:false "of_string" @@ block of_string_function;
      ]
  in
  {module_name; signature; implementation}

let rec generate_message : options:options -> string -> Message.t -> Code.module_ =
 fun ~options syntax {module_name; enums; messages; field_groups; _} ->
  let type_to_ocaml_type : Protobuf.field_data_type -> string = function
    | String_t -> "string"
    | Bytes_t -> "string"
    | Int32_t -> "int"
    | Int64_t -> "int"
    | Sint32_t -> "int"
    | Sint64_t -> "int"
    | Uint32_t -> "int"
    | Uint64_t -> "int"
    | Fixed32_t -> "int"
    | Fixed64_t -> "int"
    | Sfixed32_t -> "int"
    | Sfixed64_t -> "int"
    | Float_t -> "float"
    | Double_t -> "float"
    | Bool_t -> "bool"
    | Message_t name -> name |> Module_path.to_string |> Printf.sprintf "%s.t"
    | Enum_t name -> name |> Module_path.to_string |> Printf.sprintf "%s.t"
  in
  let generate_oneof : options:options -> Field.group -> Code.module_ option =
   fun ~options -> function
    | Field.Single _ -> None
    | Oneof {module_name; fields; _} ->
        let type_declaration =
          Code.make_variant_type
            ~options
            "t"
            (List.map fields ~f:(fun Field.{variant_name; data_type; _} ->
                 let variant_name = Variant_name.to_string variant_name in
                 Printf.sprintf "%s of %s" variant_name (type_to_ocaml_type data_type)))
        in
        Some
          {
            module_name;
            signature =
              [
                type_declaration;
                fields
                |> List.map ~f:(fun Field.{field_name; data_type; _} ->
                       let field_name = Field_name.to_string field_name in
                       Printf.sprintf
                         "val %s : %s -> t"
                         field_name
                         (type_to_ocaml_type data_type))
                |> Code.lines;
              ];
            implementation =
              [
                type_declaration;
                fields
                |> List.map ~f:(fun Field.{field_name; variant_name; _} ->
                       let field_name = Field_name.to_string field_name in
                       let variant_name = Variant_name.to_string variant_name in
                       Printf.sprintf "let %s value = %s value" field_name variant_name)
                |> Code.lines;
              ];
          }
  in
  let oneofs = field_groups |> List.filter_map ~f:(generate_oneof ~options) in
  let enums = List.map enums ~f:(generate_enum ~options) in
  let messages = List.map messages ~f:(generate_message syntax ~options) in
  let type_declaration =
    let to_record_field Field.{field_name; data_type; repeated; _} =
      let suffix =
        match repeated with
        | true -> " list"
        | false -> (
          match data_type, syntax with
          | Message_t _, _ -> " option"
          | _, "proto3" -> ""
          | Enum_t _, _ -> ""
          | _ -> " option")
      in
      ( Field_name.to_string field_name,
        Printf.sprintf "%s%s" (type_to_ocaml_type data_type) suffix )
    in
    field_groups
    |> List.map ~f:(function
           | Field.Single field -> to_record_field field
           | Oneof {module_name; field_name; _} ->
               ( Field_name.to_string field_name,
                 Printf.sprintf "%s.t option" (Module_name.to_string module_name) ))
    |> Code.make_record_type_or_unit ~options "t"
  in
  let type_to_constructor : Protobuf.field_data_type -> string = function
    | Protobuf.String_t -> "String_t"
    | Bytes_t -> "Bytes_t"
    | Int32_t -> "Int32_t"
    | Int64_t -> "Int64_t"
    | Sint32_t -> "Sint32_t"
    | Sint64_t -> "Sint64_t"
    | Uint32_t -> "Uint32_t"
    | Uint64_t -> "Uint64_t"
    | Fixed32_t -> "Fixed32_t"
    | Fixed64_t -> "Fixed64_t"
    | Sfixed32_t -> "Sfixed32_t"
    | Sfixed64_t -> "Sfixed64_t"
    | Float_t -> "Float_t"
    | Double_t -> "Double_t"
    | Bool_t -> "Bool_t"
    | Message_t name -> Module_path.to_string name
    | Enum_t name -> Module_path.to_string name
  in
  let fn_name_part_of_repeated data_type repeated =
    match repeated with
    | true -> "_repeated"
    | false -> (
      match syntax with
      | "proto3" -> ""
      | _ -> (
        match data_type with
        | Protobuf.Message_t _ | Enum_t _ -> ""
        | _ -> "_optional"))
  in
  let generate_serialization_function names field_to_id serialized_enum_type =
    Code.make_let ~recursive:true names.Generated_code.serialize
    @@
    let argument =
      match List.length field_groups with
      | 0 -> "()"
      | _ ->
          field_groups
          |> List.map ~f:(function
                 | Field.Single {field_name; _} -> field_name
                 | Oneof {field_name; _} -> field_name)
          |> List.map ~f:Field_name.to_string
          |> String.concat ~sep:"; "
          |> Printf.sprintf "{ %s }"
    in
    let body =
      let field_to_serialization_call ~for_oneof
          (Field.{field_name; data_type; repeated; _} as field)
        =
        let field_name = Field_name.to_string field_name in
        match data_type with
        | Message_t _ ->
            Printf.sprintf
              {|%s.serialize%s_user%s_field %s %s.%s %s %s|}
              names.module_alias
              (fn_name_part_of_repeated data_type repeated)
              (match for_oneof with
              | true -> "_oneof"
              | false -> "")
              (field_to_id field)
              (type_to_constructor data_type)
              names.serialize
              field_name
              names.byte_output_var_name
        | Enum_t _ ->
            Printf.sprintf
              {|%s.serialize%s_enum_field %s %s.to_%s %s %s|}
              names.module_alias
              (fn_name_part_of_repeated data_type repeated)
              (field_to_id field)
              (type_to_constructor data_type)
              serialized_enum_type
              field_name
              names.byte_output_var_name
        | _ ->
            Printf.sprintf
              {|%s.serialize%s_field %s %s.%s %s %s|}
              names.module_alias
              (fn_name_part_of_repeated data_type repeated)
              (field_to_id field)
              Generated_code.field_value_module_alias
              (type_to_constructor data_type)
              field_name
              names.byte_output_var_name
      in
      let suffix = " >>= fun () ->" in
      Code.(
        block
          [
            line
            @@ Printf.sprintf
                 "let %s = Runtime.Byte_output.create () in"
                 names.byte_output_var_name;
            List.map field_groups ~f:(function
                | Field.Single field ->
                    Printf.sprintf
                      "%s%s"
                      (field_to_serialization_call ~for_oneof:false field)
                      suffix
                    |> line
                | Oneof {field_name; fields; _} ->
                    let field_name = Field_name.to_string field_name in
                    Code.make_match ~bracketed:true ~suffix field_name
                    @@ List.concat
                         [
                           ["None", "Ok ()"];
                           List.map
                             fields
                             ~f:(fun ({variant_name; field_name; _} as field) ->
                               let field_name = Field_name.to_string field_name in
                               let variant_name = Variant_name.to_string variant_name in
                               ( Printf.sprintf "Some %s %s" variant_name field_name,
                                 field_to_serialization_call ~for_oneof:true field ));
                         ])
            |> block ~indented:false;
            line
            @@ Printf.sprintf
                 "Ok (Runtime.Byte_output.contents %s)"
                 names.byte_output_var_name;
          ])
    in
    Code.make_lambda argument body
  in
  let generate_deserialization_function names field_to_id serialized_enum_type =
    Code.make_let ~recursive:true names.Generated_code.deserialize
    @@
    let argument = "input'" in
    let result_match =
      let field_to_deserialization_call ~for_oneof
          (Field.{data_type; repeated; _} as field)
        =
        match data_type with
        | Message_t _ ->
            Printf.sprintf
              {|%s.decode%s_user%s_field %s %s.%s %s|}
              names.module_alias
              (fn_name_part_of_repeated data_type repeated)
              (match for_oneof with
              | true -> "_oneof"
              | false -> "")
              (field_to_id field)
              (type_to_constructor data_type)
              names.deserialize
              names.parsed_message_var_name
        | Enum_t _ ->
            Printf.sprintf
              {|%s.decode%s_enum_field %s %s.of_%s %s.default %s|}
              names.module_alias
              (fn_name_part_of_repeated data_type repeated)
              (field_to_id field)
              (type_to_constructor data_type)
              serialized_enum_type
              (type_to_constructor data_type)
              names.parsed_message_var_name
        | _ ->
            Printf.sprintf
              "%s.decode%s_field %s %s.%s %s"
              names.module_alias
              (fn_name_part_of_repeated data_type repeated)
              (field_to_id field)
              Generated_code.field_value_module_alias
              (type_to_constructor data_type)
              names.parsed_message_var_name
      in
      let with_suffix = Printf.sprintf "%s >>= fun %s ->" in
      Code.(
        block
          [
            line (Printf.sprintf "Ok (Runtime.Byte_input.create %s) >>=" argument);
            line
              (Printf.sprintf
                 "%s.deserialize_message >>= fun %s ->"
                 names.module_alias
                 names.parsed_message_var_name);
            List.map field_groups ~f:(function
                | Field.Single ({field_name; _} as field) ->
                    let field_name = Field_name.to_string field_name in
                    with_suffix
                      (field_to_deserialization_call ~for_oneof:false field)
                      field_name
                    |> line
                | Oneof {module_name; field_name; fields; _} ->
                    let field_name = Field_name.to_string field_name in
                    block
                      [
                        Printf.sprintf "%s.decode_oneof_field [" names.module_alias
                        |> line;
                        fields
                        |> List.map ~f:(fun field ->
                               Printf.sprintf
                                 "%s, (fun %s -> %s >>| %s.%s);"
                                 (field_to_id field)
                                 names.parsed_message_var_name
                                 (field_to_deserialization_call ~for_oneof:true field)
                                 (Module_name.to_string module_name)
                                 (Field_name.to_string field.field_name)
                               |> line)
                        |> block;
                        with_suffix
                          (Printf.sprintf "] %s" names.parsed_message_var_name)
                          field_name
                        |> line;
                      ])
            |> block ~indented:false;
            field_groups
            |> List.map ~f:(function
                   | Field.Single {field_name; _} -> field_name
                   | Oneof {field_name; _} -> field_name)
            |> List.map ~f:Field_name.to_string
            |> make_record_or_unit ~prefix:"Ok";
          ])
    in
    let body = [result_match] |> Code.block ~indented:false in
    Code.make_lambda argument body
  in
  let generate_functions
      (Generated_code.{serialize; deserialize; module_alias; _} as names) id_type_name
      field_to_id
    =
    let signatures =
      [
        Printf.sprintf
          "val %s : t -> (string, [> %s.serialization_error]) result"
          serialize
          module_alias;
        Printf.sprintf
          "val %s : string -> (t, [> %s.deserialization_error]) result"
          deserialize
          module_alias;
      ]
      |> List.map ~f:(fun s -> Code.(block [line s]))
    in
    let implementation =
      [
        generate_serialization_function names field_to_id id_type_name;
        generate_deserialization_function names field_to_id id_type_name;
      ]
    in
    signatures, implementation
  in
  let function_code =
    Generated_code.
      [
        (Binary, "int", fun Protobuf.Field.{number; _} -> Printf.sprintf "%d" number);
        ( Text,
          "string",
          fun Protobuf.Field.{original_name; _} ->
            original_name |> Printf.sprintf {|"%s"|} );
      ]
    |> List.map ~f:(fun (format, id_type_name, field_to_id) ->
           Generated_code.names_of_output_format format, id_type_name, field_to_id)
    |> List.map ~f:(fun (names, id_type_name, field_to_id) ->
           generate_functions names id_type_name field_to_id)
  in
  let signature =
    Code.(
      List.concat
        [
          [
            oneofs |> make_modules ~recursive:false ~with_implementation:false |> block;
            enums |> make_modules ~recursive:false ~with_implementation:false |> block;
            messages |> make_modules ~recursive:true ~with_implementation:false |> block;
            type_declaration;
          ];
          function_code |> List.map ~f:fst |> List.concat;
        ])
  in
  let implementation =
    Code.(
      List.concat
        [
          [
            oneofs |> make_modules ~recursive:false ~with_implementation:true |> block;
            enums |> make_modules ~recursive:false ~with_implementation:true |> block;
            messages |> make_modules ~recursive:true ~with_implementation:true |> block;
            type_declaration;
          ];
          function_code |> List.map ~f:snd |> List.concat;
        ])
  in
  {module_name; signature; implementation}

let rec generate_dependency_module package files =
  match package with
  | [] ->
      List.map files ~f:(fun File.{module_name; _} ->
          module_name |> Module_name.to_string |> Printf.sprintf "include %s")
      |> Code.lines ~indented:false
  | package_module :: rest ->
      Code.block
        ~indented:(List.length rest <> List.length files)
        [
          package_module
          |> Module_name.to_string
          |> Printf.sprintf "module %s = struct"
          |> Code.line;
          Code.block [generate_dependency_module rest files];
          Code.line "end";
        ]

let generate_file : options:options -> File.t -> Generated_code.File.t option =
 fun ~options {should_be_generated; file_name; enums; messages; syntax; _} ->
  match should_be_generated with
  | false -> None
  | true ->
      let binary_format_names = Generated_code.names_of_output_format Binary in
      let text_format_names = Generated_code.names_of_output_format Text in
      let contents =
        Code.(
          make_file
            [
              line {|[@@@ocaml.warning "-39"]|};
              line "let (>>=) = Runtime.Result.(>>=)";
              line "let (>>|) = Runtime.Result.(>>|)";
              line
              @@ Printf.sprintf
                   "module %s = Runtime.%s"
                   Generated_code.field_value_module_alias
                   Generated_code.field_value_runtime_module_name;
              line
              @@ Printf.sprintf
                   "module %s = Runtime.%s"
                   binary_format_names.module_alias
                   binary_format_names.runtime_module_name;
              line
              @@ Printf.sprintf
                   "module %s = Runtime.%s"
                   text_format_names.module_alias
                   text_format_names.runtime_module_name;
              List.map enums ~f:(generate_enum ~options)
              |> Code.make_modules ~recursive:false ~with_implementation:true
              |> block ~indented:false;
              List.map messages ~f:(generate_message ~options syntax)
              |> Code.make_modules ~recursive:true ~with_implementation:true
              |> block ~indented:false;
            ])
        |> Code.emit
      in
      Some {file_name; contents}

let generate_files : options:options -> Protobuf.t -> Generated_code.t =
 fun ~options {files} -> List.filter_map ~f:(generate_file ~options) files
