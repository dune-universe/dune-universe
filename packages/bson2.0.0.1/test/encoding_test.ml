open OUnit
(*open Core *)
open Validators
open Bson2

let name = "Test bson encoder"

let tests =
    [ "Test flat document" >::
      (fun _ ->
          validate_size Fixture.flat_doc Fixture.size_of_flat_doc;
          validate_expected_contents Fixture.flat_doc_bytes Fixture.flat_doc)
    ; "Test nested document" >::
      (fun _ ->
          validate_size Fixture.nested_doc Fixture.size_of_nested_doc;
          validate_expected_contents Fixture.nested_doc_bytes Fixture.nested_doc)
    ; "Test nested document'" >::
       (fun _ ->
        validate_size Fixture.nested_doc' Fixture.size_of_nested_doc')
    ; "Test binary fields" >::
        (fun _ ->
            validate_size Fixture.binary_fields_doc Fixture.size_of_binary_fields_doc;
            validate_expected_contents Fixture.binary_fields_doc_bytes Fixture.binary_fields_doc)
    ; "Test closing document or array in invalid state throws exception." >::
        (fun _ ->
            let open Binary.Writer in
            let doc =
                create 64 in
            write_string doc "foo" "bar";
            write_document_close doc;
            try
                write_document_close doc;
                assert_failure "Closing an already-closed document did not raise an exception" 
            with Invalid_state _ ->
                ())
    ; "Test cannot close a document while an array or js code scope is open" >::
        (fun _ ->
            let open Binary.Writer in
            let doc =
                create 64 in
            write_array_start doc "array_field";
            write_string doc "0" "foo";
            (try
                write_document_close doc;
                assert_failure "Closing a document with an open array did not raise an exception"
            with Invalid_state _ ->
                write_array_close doc;
                write_document_close doc);
                
            let doc =
                create 64 in
            write_js_with_scope doc "js_code_w_scope_field" "function() { return 5; }";
            write_string doc "js_scope_field" "y";
            write_js_with_scope_close doc;
            write_document_close doc)
    ]
