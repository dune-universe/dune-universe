open Base

let unwrap ~expected option =
  let message = Printf.sprintf "Expected %s but got None" expected in
  Option.value_exn ~message option

module Plugin = struct
  include Protocell_google.Google_protobuf_compiler_plugin_pc

  let decode_request = Code_generator_request.of_binary

  let encode_response = Code_generator_response.to_binary

  let error_response =
    let bytes_result =
      Code_generator_response.to_binary {error = Some "Protocell error"; file = []}
    in
    unwrap ~expected:"serialized protocell error" (Result.ok bytes_result)
end

module Descriptor = struct
  include Protocell_google.Google_protobuf_descriptor_pc
end

module Protobuf = struct
  include Shared.Protobuf

  let module_name_of_string_opt input =
    input |> Option.bind ~f:Module_name.of_string |> unwrap ~expected:"module_name"

  let variant_name_of_string_opt input =
    input |> Option.bind ~f:Variant_name.of_string |> unwrap ~expected:"variant_name"

  let field_name_of_string_opt input =
    input |> Option.bind ~f:Field_name.of_string |> unwrap ~expected:"field_name"

  let field_type_of_request
      :  package:Module_path.t ->
      substitutions:(Module_path.t, Module_path.t) Hashtbl.t ->
      Descriptor.Field_descriptor_proto.t -> field_data_type
    =
   fun ~package ~substitutions {type'; type_name; _} ->
    let determine_module_path type_name =
      let full_path =
        type_name
        |> unwrap ~expected:"type_name"
        |> String.chop_prefix_exn ~prefix:"."
        |> Module_path.of_string
        |> unwrap ~expected:"module_path"
      in
      match Hashtbl.find substitutions full_path with
      | Some subst_path -> subst_path
      | None -> List.drop full_path (List.length package)
    in
    match type' with
    | Type_string -> String_t
    | Type_bytes -> Bytes_t
    | Type_int32 -> Int32_t
    | Type_int64 -> Int64_t
    | Type_sint32 -> Sint32_t
    | Type_sint64 -> Sint64_t
    | Type_uint32 -> Uint32_t
    | Type_uint64 -> Uint64_t
    | Type_fixed32 -> Fixed32_t
    | Type_fixed64 -> Fixed64_t
    | Type_sfixed32 -> Sfixed32_t
    | Type_sfixed64 -> Sfixed64_t
    | Type_float -> Float_t
    | Type_double -> Double_t
    | Type_bool -> Bool_t
    | Type_message -> Message_t (determine_module_path type_name)
    | Type_enum -> Enum_t (determine_module_path type_name)
    | Type_group -> failwith "Groups are not supported"

  let enum_of_request : Descriptor.Enum_descriptor_proto.t -> Enum.t =
   fun {name; value'; _} ->
    let values =
      List.map value' ~f:(fun {name; number; _} ->
          Enum.
            {
              id = unwrap ~expected:"enum_value_id" number;
              original_name = unwrap ~expected:"enum_value_name" name;
              variant_name = variant_name_of_string_opt name;
            })
    in
    {module_name = module_name_of_string_opt name; values}

  let field_of_request
      :  package:Module_path.t ->
      substitutions:(Module_path.t, Module_path.t) Hashtbl.t ->
      Descriptor.Field_descriptor_proto.t -> Field.t
    =
   fun ~package ~substitutions ({name; number; label; oneof_index; _} as field) ->
    {
      original_name = unwrap ~expected:"original_field_name" name;
      field_name = field_name_of_string_opt name;
      variant_name = variant_name_of_string_opt name;
      number = unwrap ~expected:"field_number" number;
      data_type = field_type_of_request ~package ~substitutions field;
      repeated =
        (match label with
        | Descriptor.Field_descriptor_proto.Label.Label_repeated -> true
        | _ -> false);
      oneof_index;
    }

  let oneof_of_request : Descriptor.Oneof_descriptor_proto.t -> Oneof.t =
   fun {name; _} ->
    {
      module_name = module_name_of_string_opt name;
      field_name = field_name_of_string_opt name;
    }

  let rec message_of_request
      :  package:Module_path.t ->
      substitutions:(Module_path.t, Module_path.t) Hashtbl.t ->
      Descriptor.Descriptor_proto.t -> Message.t
    =
   fun ~package ~substitutions {name; field; nested_type; enum_type; oneof_decl; _} ->
    let fields = List.map field ~f:(field_of_request ~package ~substitutions) in
    let oneofs = List.map oneof_decl ~f:oneof_of_request in
    {
      module_name = module_name_of_string_opt name;
      enums = List.map enum_type ~f:enum_of_request;
      messages = List.map nested_type ~f:(message_of_request ~package ~substitutions);
      field_groups = Field.determine_groups fields oneofs;
    }

  let embedded_packages =
    ["google.protobuf"; "google.protobuf.compiler"]
    |> List.map ~f:Module_path.of_string
    |> Option.all
    |> unwrap ~expected:"embedded_packages"

  let is_embedded File.{package; _} =
    List.mem embedded_packages package ~equal:Module_path.equal

  let protocell_embedded_code_module =
    "protocell_google"
    |> Module_name.of_string
    |> unwrap ~expected:"protocell_embedded_code_module"

  let file_of_request
      :  files_seen:(string, File.t) List.Assoc.t -> should_be_generated:bool ->
      Descriptor.File_descriptor_proto.t -> File.t
    =
   fun ~files_seen
       ~should_be_generated
       {name; package; enum_type; message_type; dependency; syntax; _} ->
    let dependencies =
      List.filter_map files_seen ~f:(fun (file_name, file) ->
          match List.exists dependency ~f:(String.equal file_name) with
          | true -> Some file
          | false -> None)
    in
    let substitutions =
      let enum_module_paths ~from_prefix ~to_prefix enums =
        List.map enums ~f:(fun Enum.{module_name; _} ->
            ( List.concat [from_prefix; [module_name]],
              List.concat [to_prefix; [module_name]] ))
      in
      let rec message_module_paths ~from_prefix ~to_prefix messages =
        List.concat_map messages ~f:(fun Message.{module_name; messages; enums; _} ->
            let from_prefix = List.concat [from_prefix; [module_name]] in
            let to_prefix = List.concat [to_prefix; [module_name]] in
            List.concat
              [
                [from_prefix, to_prefix];
                message_module_paths ~from_prefix ~to_prefix messages;
                enum_module_paths ~from_prefix ~to_prefix enums;
              ])
      in
      dependencies
      |> List.filter_map ~f:(fun (File.{should_be_generated; module_name; _} as file') ->
             match should_be_generated, is_embedded file' with
             | true, _ -> Some ([module_name], file')
             | false, true -> Some ([protocell_embedded_code_module; module_name], file')
             | false, false -> None)
      |> List.concat_map ~f:(fun (to_prefix, File.{enums; package; messages; _}) ->
             List.concat
               [
                 message_module_paths ~from_prefix:package ~to_prefix messages;
                 enum_module_paths ~from_prefix:package ~to_prefix enums;
               ])
      |> Hashtbl.of_alist_exn (module Module_path)
    in
    let ocaml_module_name name =
      let base_name =
        match String.chop_suffix name ~suffix:".proto" with
        | None -> name
        | Some stem -> stem
      in
      Printf.sprintf "%s_pc" base_name |> String.tr ~target:'/' ~replacement:'_'
    in
    let package =
      match package with
      | None -> []
      | Some package ->
          package |> Module_path.of_string |> unwrap ~expected:"module_path"
    in
    let file_name =
      Printf.sprintf "%s.ml" (ocaml_module_name (unwrap ~expected:"file_name" name))
    in
    let module_name =
      Option.(
        name
        >>| ocaml_module_name
        >>= Module_name.of_string
        |> unwrap ~expected:"module_name")
    in
    let enums = List.map ~f:enum_of_request enum_type in
    let messages =
      List.map ~f:(message_of_request ~package ~substitutions) message_type
    in
    let syntax = Option.value syntax ~default:"proto2" in
    {
      file_name;
      package;
      module_name;
      enums;
      messages;
      dependencies;
      syntax;
      should_be_generated;
    }

  let of_request : Plugin.Code_generator_request.t -> t =
   fun {proto_file; file_to_generate; _} ->
    let files =
      proto_file
      |> List.fold
           ~init:([], [])
           ~f:(fun (files_seen, accumulator)
                   (Descriptor.File_descriptor_proto.{name; _} as proto)
                   ->
             let name = unwrap ~expected:"file_name" name in
             let should_be_generated =
               List.mem file_to_generate name ~equal:String.equal
             in
             let file' = file_of_request ~files_seen ~should_be_generated proto in
             let file_name = unwrap ~expected:"file_name" proto.name in
             let files_seen = (file_name, file') :: files_seen in
             files_seen, file' :: accumulator)
      |> snd
    in
    {files}
end

module Generated_code = struct
  include Shared.Generated_code

  let file_to_response : File.t -> Plugin.Code_generator_response.File.t =
   fun {file_name; contents} ->
    {name = Some file_name; insertion_point = None; content = Some contents}

  let to_response : t -> Plugin.Code_generator_response.t =
   fun files -> {error = None; file = List.map ~f:file_to_response files}
end
