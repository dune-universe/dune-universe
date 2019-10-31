open Core
open OUnit
open Bson2

let bytes_to_int32 b bytes_offset =
    let s = Bytes.sub b ~pos:bytes_offset ~len:(bytes_offset + 4) in
    let rec helper i acc =
        if i < 0
        then acc
        else
            let high =
                Bytes.get s i
                |> Char.to_int
                |> Int32.of_int_exn in
            let acc =
                Int32.(shift_left acc 8 lor high) in
            helper (i - 1) acc in
    helper 3 0l

let validate_size ?offset:(o=0) doc expected_size =
    let encoded_size =
        bytes_to_int32 doc o
        |> Int32.to_int_exn in
    let msg = sprintf "Expected document size of %d bytes but got size of %d bytes" expected_size encoded_size in
    assert_equal ~msg:msg encoded_size expected_size;
    let doc_size = Bytes.length doc in
    let msg = sprintf "Document size was encoded as %d bytes but actual document length was %d" encoded_size doc_size in
    assert_equal ~msg:msg doc_size expected_size

let string_of_read_result r =
    Binary.Reader.sexp_of_read_result r
    |> Sexp.to_string

let validate_read_result expected actual =
    let message =
        sprintf "Expected %s but got %s" (string_of_read_result expected) (string_of_read_result actual) in
    assert_equal expected actual ~msg:message

let validate_expected_read_results doc read_result_validators =
    let reader = Binary.Reader.of_bytes doc in
    List.iter
        read_result_validators
        ~f:(fun validator -> Binary.Reader.read_next reader |> validator)

let validate_expected_contents expected actual =
    let expected_size = Bytes.length expected in
    let actual_size = Bytes.length actual in
    let msg = sprintf "Expected document size of %d but got size of %d bytes" expected_size actual_size in
    assert_equal ~msg:msg expected_size actual_size;
    Bytes.to_list actual
    |> List.iteri
        ~f:(fun i b ->
            let b' = Bytes.get expected i in
            let msg = sprintf "Expected char %c but got %c at position %d" b' b i in
            assert_equal ~msg:msg b' b)
