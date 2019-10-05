open Core

type process_error = [`Process_execution_error of string]

let execute_process_with_input ~prog ~args ~input =
  let process = Unix.create_process ~prog ~args in
  let standard_out = Unix.in_channel_of_descr process.stdout in
  let standard_in = Unix.out_channel_of_descr process.stdin in
  let () = Stdio.Out_channel.output_string standard_in input in
  let () = Stdio.Out_channel.close standard_in in
  let output = Stdio.In_channel.input_all standard_out in
  let code_or_signal = Unix.waitpid process.pid in
  let () = Stdio.In_channel.close standard_out in
  let () = Unix.close process.stderr in
  match Unix.Exit_or_signal.or_error code_or_signal with
  | Ok () -> Ok output
  | Error error -> Error (`Process_execution_error (Error.to_string_hum error))

module type Serdes_testable = sig
  type t [@@deriving eq, show]

  val to_binary : t -> (string, [> Runtime.Binary_format.serialization_error]) result

  val of_binary : string -> (t, [> Runtime.Binary_format.deserialization_error]) result

  val to_text : t -> (string, [> Runtime.Text_format.serialization_error]) result

  val of_text : string -> (t, [> Runtime.Text_format.deserialization_error]) result
end

let byte_input_error_to_string = function
  | `Not_enough_bytes -> "Unexpected end of input"

let field_validation_error_to_string = function
  | `Integer_outside_field_type_range (typ, int) ->
      Printf.sprintf
        "Integer %d is outside of the range of field type %s"
        int
        (Runtime.Field_value.typ_to_string typ)

let binary_format_parse_error_to_string = function
  | `Unknown_wire_type int -> Printf.sprintf "Unknown wire type ID %d" int
  | `Varint_too_long -> "Varint value was longer than 64 bits"
  | `Invalid_string_length int -> Printf.sprintf "Invalid string length: %d" int
  | `Integer_outside_int_type_range int64 ->
      Printf.sprintf "Varint value %s outside OCaml int type range"
      @@ Int64.to_string int64
  | #Runtime.Byte_input.error as e -> byte_input_error_to_string e

let binary_format_deserialization_error_to_string error =
  let wrong_sort_msg field_type_name sort typ =
    Printf.sprintf
      "%s field type %s cannot accept value type %s"
      field_type_name
      (Runtime.Field_value.typ_to_string typ)
      (Runtime.Binary_format.sort_to_string sort)
  in
  match error with
  | #Runtime.Binary_format.parse_error as e -> binary_format_parse_error_to_string e
  | `Wrong_value_sort_for_string_field (sort, typ) -> wrong_sort_msg "String" sort typ
  | `Wrong_value_sort_for_int_field (sort, typ) -> wrong_sort_msg "Integer" sort typ
  | `Wrong_value_sort_for_float_field (sort, typ) -> wrong_sort_msg "Float" sort typ
  | `Wrong_value_sort_for_bool_field (sort, typ) -> wrong_sort_msg "Boolean" sort typ
  | `Wrong_value_sort_for_user_field sort ->
      Printf.sprintf
        "Message field type cannot accept value type %s"
        (Runtime.Binary_format.sort_to_string sort)
  | `Wrong_value_sort_for_enum_field sort ->
      Printf.sprintf
        "Enum field type cannot accept value type %s"
        (Runtime.Binary_format.sort_to_string sort)
  | `Unrecognized_enum_value -> "Unrecognized enum value"
  | `Multiple_oneof_fields_set -> "Multiple oneof fields set"
  | #Runtime.Field_value.validation_error as e -> field_validation_error_to_string e

let process_error_to_string = function
  | `Process_execution_error message ->
      Printf.sprintf "Process execution error: %s" message

let binary_error_to_string = function
  | #process_error as e -> process_error_to_string e
  | #Runtime.Binary_format.deserialization_error as e ->
      binary_format_deserialization_error_to_string e

let text_format_parse_error_to_string = function
  | `Unexpected_character char -> Printf.sprintf "Unexpected character: %c" char
  | `Invalid_number_string string -> Printf.sprintf "Invalid number string: %s" string
  | `Identifier_expected -> "Identifier expected"
  | `Nested_message_unfinished -> "Nested message unfinished"
  | #Runtime.Byte_input.error as e -> byte_input_error_to_string e

let text_format_deserialization_error_to_string error =
  let wrong_sort_msg field_type_name sort typ =
    Printf.sprintf
      "%s field type %s cannot accept value type %s"
      field_type_name
      (Runtime.Field_value.typ_to_string typ)
      (Runtime.Text_format.sort_to_string sort)
  in
  match error with
  | #Runtime.Text_format.parse_error as e -> text_format_parse_error_to_string e
  | `Wrong_value_sort_for_string_field (sort, typ) -> wrong_sort_msg "String" sort typ
  | `Wrong_value_sort_for_int_field (sort, typ) -> wrong_sort_msg "Integer" sort typ
  | `Wrong_value_sort_for_float_field (sort, typ) -> wrong_sort_msg "Float" sort typ
  | `Wrong_value_sort_for_bool_field (sort, typ) -> wrong_sort_msg "Boolean" sort typ
  | `Wrong_value_sort_for_user_field sort ->
      Printf.sprintf
        "Message field type cannot accept value type %s"
        (Runtime.Text_format.sort_to_string sort)
  | `Wrong_value_sort_for_enum_field sort ->
      Printf.sprintf
        "Enum field type cannot accept value type %s"
        (Runtime.Text_format.sort_to_string sort)
  | `Integer_outside_int_type_range int64 ->
      Printf.sprintf "Varint value %s outside OCaml int type range"
      @@ Int64.to_string int64
  | `Unrecognized_enum_value -> "Unrecognized enum value"
  | `Multiple_oneof_fields_set -> "Multiple oneof fields set"
  | #Runtime.Field_value.validation_error as e -> field_validation_error_to_string e

let text_error_to_string = function
  | #process_error as e -> process_error_to_string e
  | #Runtime.Text_format.deserialization_error as e ->
      text_format_deserialization_error_to_string e

let make_tests title test_fn values_to_test =
  let value_count = List.length values_to_test in
  List.mapi values_to_test ~f:(fun index value ->
      let test_name = Printf.sprintf "%s (%d/%d)" title index (value_count - 1) in
      Alcotest.test_case test_name `Quick @@ test_fn value)

let invariant_suite (type t) (module T : Serdes_testable with type t = t)
    protobuf_type_name values_to_test
  =
  let t_testable : T.t Alcotest.testable = Alcotest.testable T.pp T.equal in
  let protobuf_file_name = "test.proto" in
  let binary_format_roundtrip value () =
    let open Result.Let_syntax in
    match value |> T.to_binary >>= T.of_binary with
    | Ok actual ->
        Alcotest.(check t_testable "to_binary |> of_binary mismatch" value actual)
    | Error e -> Alcotest.fail (binary_error_to_string e)
  in
  let text_format_roundtrip value () =
    let open Result.Let_syntax in
    match value |> T.to_text >>= T.of_text with
    | Ok actual -> Alcotest.(check t_testable "to_text |> of_text mismatch" value actual)
    | Error e -> Alcotest.fail (text_error_to_string e)
  in
  let decode_protoc_binary_output value () =
    let open Result.Let_syntax in
    match
      T.to_text value
      >>= (fun input ->
            execute_process_with_input
              ~prog:"protoc"
              ~args:[Printf.sprintf "--encode=%s" protobuf_type_name; protobuf_file_name]
              ~input)
      >>= T.of_binary
    with
    | Ok actual ->
        Alcotest.(
          check t_testable "to_text |> protoc |> of_binary mismatch" value actual)
    | Error e -> Alcotest.fail (binary_error_to_string e)
  in
  let generate_protoc_binary_input value () =
    let open Result.Let_syntax in
    match
      T.to_binary value
      >>= (fun input ->
            execute_process_with_input
              ~prog:"protoc"
              ~args:[Printf.sprintf "--decode=%s" protobuf_type_name; protobuf_file_name]
              ~input)
      >>= T.of_text
    with
    | Ok actual ->
        Alcotest.(
          check t_testable "to_binary |> protoc |> of_text mismatch" value actual)
    | Error e -> Alcotest.fail (text_error_to_string e)
  in
  ( protobuf_type_name,
    [
      make_tests "Invariant: to_binary |> of_binary is identity" binary_format_roundtrip;
      make_tests "Invariant: to_text |> of_text is identity" text_format_roundtrip;
      make_tests
        "Invariant: to_text |> protoc |> of_binary is identity"
        decode_protoc_binary_output;
      make_tests
        "Invariant: to_binary |> protoc |> of_text is identity"
        generate_protoc_binary_input;
    ]
    |> List.map ~f:(fun test -> test values_to_test)
    |> List.concat )
