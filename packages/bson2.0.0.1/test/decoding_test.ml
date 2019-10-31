open OUnit
open Core
open Validators
open Bson2

let name = "Test bson decoder"

let validators =
    let open Binary.Reader in
    [ Field("foo", Int32(5l))
    ; Field("field1", Int64(123456L))
    ; Field("biggerfield", Boolean(true))
    ; End_of_document]
    |> List.map ~f:validate_read_result

let nested_doc_validators =
    let open Binary.Reader in
    [ Field("nested_doc", Document_start)
    ; Field("foo", Int32(5l))
    ; Field("field1", Int64(123456L))
    ; Field("biggerfield", Boolean(true))
    ; End_of_document
    ; End_of_document ]
    |> List.map ~f:validate_read_result

let nested_doc'_validators =
    let open Binary.Reader in
    [ Field("foo", Int32(5l))
    ; Field("field1", Int64(123456L))
    ; Field("biggerfield", Boolean(true))
    ; Field("inner_doc", Document_start)
    ; Field("foo", Int32(5l))
    ; Field("field1", Int64(123456L))
    ; Field("biggerfield", Boolean(true))
    ; End_of_document
    ; End_of_document ]
    |> List.map ~f:validate_read_result

let binary_doc_validators =
    let open Binary in
    let open Reader in
    [ Field("generic_field", Binary(Generic, Bytes.of_string "generic_value"))
    ; Field("function_field", Binary(Function, Bytes.of_string "function_value"))
    ; Field("binary_old_field", Binary(Binary_old, Bytes.of_string "binary_old_value"))
    ; Field("uuid_old_field", Binary(UUID_old, Bytes.of_string "uuid_old_value"))
    ; Field("uuid_field", Binary(UUID, Bytes.of_string "uuid_value")) 
    ; Field("md5_field", Binary(MD5, Bytes.of_string "md5_value"))
    ; Field("encrypted_field", Binary(Encrypted, Bytes.of_string "encrypted_value"))
    ; Field("user_defined_value", Binary(User_defined, Bytes.of_string "user_defined"))
    ; End_of_document ]
    |> List.map ~f:validate_read_result 

let doc_with_all_fields_validators =
    let open Binary in
    let open Reader in
    [ Field("name_float", Double(123.456))
    ; Field("name_string", String("value_string"))
    ; Field("name_document", Document_start)
    ; End_of_document
    ; Field("name_array", Array_start)
    ; Field("0", String("field_0"))
    ; Field("1", String("field_1"))
    ; Field("2", String("field_2"))
    ; Field("3", String("field_3"))
    ; Field("4", String("field_4"))
    ; End_of_document
    ; Field("generic_field", Binary(Generic, (Bytes.of_string "generic_bytes")))
    ; Field("name_objectid", ObjectId(Bytes.of_string "000000000000"))
    ; Field("name_bool", Boolean(true))
    ; Field("name_datetime", DateTime(1571410334L))
    ; Field("name_null", Null)
    ; Field("name_regex", Regex({ pattern = "^foo$"; options = "imx" }))
    ; Field("name_js_code", JSCode("function(){ return 5; }"))
    ; Field("name_js_code_with_scope", JSCode_with_scope("function() { return x; }"))
    ; Field("x", String("string_x"))
    ; End_of_document
    ; Field("name_int32", Int32(123l))
    ; Field("name_timestamp", Timestamp(12345L))
    ; Field("name_int64", Int64(123456L))
    ; Field("name_decimal128", Decimal128(Bytes.of_string "0000000000000000"))
    ; Field("name_minkey", Min_key)
    ; Field("name_maxkey", Max_key)
    ; End_of_document ]
    |> List.map ~f:validate_read_result

(* TODO: abstract out validation of document size into its own function. *)
let tests =
    [ "Test flat document" >::
        (fun _ ->
            validate_expected_read_results Fixture.flat_doc_bytes validators)
    ; "Test nested document" >::
        (fun _ ->
            validate_expected_read_results Fixture.nested_doc_bytes nested_doc_validators)
    ; "Test nested document'" >::
        (fun _ ->
            validate_expected_read_results Fixture.nested_doc' nested_doc'_validators)
    ; "Test doc with all fields" >::
        (fun _ ->
            validate_expected_read_results Fixture.doc_with_all_fields doc_with_all_fields_validators)
    ; "Test binary document " >::
        (fun _ ->
            validate_expected_read_results Fixture.binary_fields_doc_bytes binary_doc_validators) ]
