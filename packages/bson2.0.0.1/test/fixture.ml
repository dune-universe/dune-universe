open Core
open Bson2


let write_flat_doc doc =
    Binary.Writer.(write_int32 doc "foo" 5l;
          write_int64 doc "field1" 123456L;
          write_bool doc "biggerfield" true)

let flat_doc =
    Binary.Writer.(
        let doc = create 64 in
        write_flat_doc doc;
        write_document_close doc;
        match to_bytes doc with
        | Ok bytes -> bytes
        | Error _ -> failwith "Error creating test document")

let flat_doc_bytes =
    [ 44; 0; 0; 0; 16; 102; 111; 111; 0; 5; 0; 0; 0; 18; 102; 105; 101; 108; 100; 49; 0; 64
    ; 226; 1; 0; 0; 0; 0; 0; 8; 98; 105; 103; 103; 101; 114; 102; 105; 101; 108; 100; 0; 1; 0 ]
    |> List.map ~f:Char.of_int_exn
    |> Bytes.of_char_list

let size_of_flat_doc =
    (* 5 + 4 = 9
     * 8 + 8 = 16
     * 13 + 1 = 14
     * 5 extra for doc encoding*)
    44

let nested_doc' =
    let open Binary.Writer in    
    let doc = create 64 in
    write_flat_doc doc;
    write_document_start doc "inner_doc";
    write_flat_doc doc;
    write_document_close doc;
    write_document_close doc;
    match to_bytes doc with
    | Ok bytes -> bytes
    | Error _ -> failwith "Error creating test document"

let size_of_nested_doc' =
    99

let nested_doc =
    let open Binary.Writer in
    let doc = create 64 in
    write_document_start doc "nested_doc";
    write_flat_doc doc;
    write_document_close doc;
    write_document_close doc;
    match to_bytes doc with
    | Ok bytes -> bytes
    | Error _ -> failwith "Error creating nested document"

let nested_doc_bytes =
    [ 61; 0; 0; 0; 3; 110; 101; 115; 116; 101; 100; 95; 100; 111; 99; 0; 44; 0
    ; 0; 0; 16; 102; 111; 111; 0; 5; 0; 0; 0; 18; 102; 105; 101; 108; 100; 49; 0
    ; 64; 226; 1; 0; 0; 0; 0; 0; 8; 98; 105; 103; 103; 101; 114; 102; 105; 101; 108; 100; 0; 1; 0; 0 ]
    |> List.map ~f:Char.of_int_exn
    |> Bytes.of_char_list

let size_of_nested_doc =
    (* 4 bytes for size of total doc
     * 1 byte for terminal of total doc
     * 12 bytes for char, fieldname of nested doc
     * 44 bytes for nested doc *)
    61

let write_string_array doc num_fields =
    let rec helper i =
    if i >= num_fields
    then ()
    else
        let key = Int.to_string i in
        let value = sprintf "field_%s" key in
        Binary.Writer.write_string doc key value;
        helper (i + 1) in
    helper 0

let doc_with_all_fields =
    let open Binary.Writer in
    let doc = create 64 in
    write_float doc "name_float" 123.456;
    write_string doc "name_string" "value_string";
    write_document_start doc "name_document";
    write_document_close doc;
    write_array_start doc "name_array";
    write_string_array doc 5; 
    write_array_close doc;
    write_binary doc "generic_field" Generic (Bytes.of_string "generic_bytes");
    write_objectid doc "name_objectid" (Bytes.of_string "000000000000");
    write_bool doc "name_bool" true;
    write_utc_datetime doc "name_datetime" 1571410334L;
    write_null doc "name_null";
    write_regex doc "name_regex" ~pattern:"^foo$" ~options:"imx";
    write_js doc "name_js_code" "function(){ return 5; }";
    write_js_with_scope doc "name_js_code_with_scope" "function() { return x; }";
    write_string doc "x" "string_x";
    write_js_with_scope_close doc;
    write_int32 doc "name_int32" 123l;
    write_timestamp doc "name_timestamp" 12345L;
    write_int64 doc "name_int64" 123456L;
    write_decimal128 doc "name_decimal128" (Bytes.of_string "0000000000000000");
    write_minkey doc "name_minkey";
    write_maxkey doc "name_maxkey";
    write_document_close doc;
    match to_bytes doc with
    | Ok bytes -> bytes
    | Error _ ->
        failwith "Error creating document with all fields"

let binary_fields_doc =
    let open Binary in
    let open Writer in
    let doc = create 64 in
    write_binary doc "generic_field" Generic (Bytes.of_string "generic_value");
    write_binary doc "function_field" Function (Bytes.of_string "function_value");
    write_binary doc "binary_old_field" Binary_old (Bytes.of_string "binary_old_value");
    write_binary doc "uuid_old_field" UUID_old (Bytes.of_string "uuid_old_value");
    write_binary doc "uuid_field" UUID (Bytes.of_string "uuid_value");
    write_binary doc "md5_field" MD5 (Bytes.of_string "md5_value");
    write_binary doc "encrypted_field" Encrypted (Bytes.of_string "encrypted_value");
    write_binary doc "user_defined_value" User_defined (Bytes.of_string "user_defined");
    write_document_close doc;
    match to_bytes doc with
    | Ok bytes -> bytes
    | Error _ -> failwith "Error creating binary_fields document"

let binary_fields_doc_bytes =
    [ 17; 1; 0; 0; 5; 103; 101; 110; 101; 114; 105; 99; 95; 102; 105; 101; 108; 100; 0; 13
    ; 0; 0; 0; 0; 103; 101; 110; 101; 114; 105; 99; 95; 118; 97; 108; 117; 101; 5; 102; 117
    ; 110; 99; 116; 105; 111; 110; 95; 102; 105; 101; 108; 100; 0; 14; 0; 0; 0; 1; 102; 117
    ; 110; 99; 116; 105; 111; 110; 95; 118; 97; 108; 117; 101; 5; 98; 105; 110; 97; 114; 121
    ; 95; 111; 108; 100; 95; 102; 105; 101; 108; 100; 0; 16; 0; 0; 0; 2; 98; 105; 110; 97
    ; 114; 121; 95; 111; 108; 100; 95; 118; 97; 108; 117; 101; 5; 117; 117; 105; 100; 95; 111
    ; 108; 100; 95; 102; 105; 101; 108; 100; 0; 14; 0; 0; 0; 3; 117; 117; 105; 100; 95; 111
    ; 108; 100; 95; 118; 97; 108; 117; 101; 5; 117; 117; 105; 100; 95; 102; 105; 101; 108; 100; 0
    ; 10; 0; 0; 0; 4; 117; 117; 105; 100; 95; 118; 97; 108; 117; 101; 5; 109; 100; 53; 95
    ; 102; 105; 101; 108; 100; 0; 9; 0; 0; 0; 5; 109; 100; 53; 95; 118; 97; 108; 117; 101
    ; 5; 101; 110; 99; 114; 121; 112; 116; 101; 100; 95; 102; 105; 101; 108; 100; 0; 15; 0; 0
    ; 0; 6; 101; 110; 99; 114; 121; 112; 116; 101; 100; 95; 118; 97; 108; 117; 101; 5; 117; 115
    ; 101; 114; 95; 100; 101; 102; 105; 110; 101; 100; 95; 118; 97; 108; 117; 101; 0; 12; 0; 0
    ; 0; 128; 117; 115; 101; 114; 95; 100; 101; 102; 105; 110; 101; 100; 0 ]
    |> List.map ~f:Char.of_int_exn
    |> Bytes.of_char_list

let size_of_binary_fields_doc =
    273
