open! Core
open! Async
open! Import

let generate_test_string ~string_length =
  String.init string_length ~f:(fun i -> Char.of_int_exn ((i % 26) + Char.to_int 'a'))
;;

let test_chunks ~string_length =
  let str = generate_test_string ~string_length in
  let chunks = Embed_file_lib.chunks str in
  let str' = String.concat chunks in
  require [%here] (String.equal str str');
  let chunk_lengths = List.map chunks ~f:String.length in
  let chunk_lengths =
    List.map chunk_lengths ~f:(fun chunk_len ->
      Ordering.of_int (Int.compare chunk_len Embed_file_lib.chunk_len))
  in
  print_s [%message (chunks : string list) (chunk_lengths : Ordering.t list)]
;;

let%expect_test "lossless chunks 1" =
  test_chunks
    ~string_length:((Embed_file_lib.chunk_len * 3) + (Embed_file_lib.chunk_len / 2));
  [%expect
    {|
    ((chunks (
       abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzab
       cdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcd
       efghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdef
       ghijklmnopqrstuvwxyzabcdefghijklmnopqrst))
     (chunk_lengths (Equal Equal Equal Less)))
  |}];
  return ()
;;

let%expect_test "lossless chunks 2" =
  test_chunks ~string_length:(Embed_file_lib.chunk_len * 3);
  [%expect
    {|
    ((chunks (
       abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzab
       cdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcd
       efghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdef))
     (chunk_lengths (Equal Equal Equal)))
  |}];
  return ()
;;

let%expect_test "lossless chunks 3" =
  test_chunks ~string_length:((Embed_file_lib.chunk_len * 3) + 1);
  [%expect
    {|
    ((chunks (
       abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzab
       cdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcd
       efghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdef
       g))
     (chunk_lengths (Equal Equal Equal Less)))
  |}];
  return ()
;;

let%expect_test "lossless chunks 4" =
  test_chunks ~string_length:((Embed_file_lib.chunk_len * 3) - 1);
  [%expect
    {|
    ((chunks (
       abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzab
       cdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcd
       efghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcde))
     (chunk_lengths (Equal Equal Less)))
  |}];
  return ()
;;

let test_output_ml contents =
  let reader =
    Pipe.create_reader ~close_on_exception:true (fun w ->
      let%bind w, `Closed_and_flushed_downstream flushed =
        Writer.of_pipe (Info.of_string "test") w
      in
      Embed_file_lib.write_ml w ~var:"foo" ~contents;
      let%bind () = Writer.close w in
      flushed)
  in
  Pipe.to_list reader >>| String.concat >>| print_endline
;;

let%expect_test "ml output" =
  let%bind () =
    test_output_ml
      (generate_test_string
         ~string_length:((Embed_file_lib.chunk_len * 3) + (Embed_file_lib.chunk_len / 2)))
  in
  [%expect
    {|
    let foo =
      "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzab\
       cdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcd\
       efghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdef\
       ghijklmnopqrstuvwxyzabcdefghijklmnopqrst"
    ;;
  |}];
  return ()
;;

let%expect_test "ml output for empty input" =
  let%bind () = test_output_ml "" in
  [%expect {|
    let foo =
      ""
    ;;
  |}];
  return ()
;;

let%expect_test "ml output with spaces for input" =
  let%bind () =
    test_output_ml
      (String.make ((Embed_file_lib.chunk_len * 3) + (Embed_file_lib.chunk_len / 2)) ' ')
  in
  [%expect
    {|
    let foo =
      "                                                                                \
      \                                                                                \
      \                                                                                \
      \                                        "
    ;;
  |}];
  return ()
;;

let%expect_test "replace_CRs" =
  List.iter [ ""; "a"; "CR"; "XCR"; "CR-soon"; "CR-someday" ] ~f:(fun input ->
    let output = Embed_file_lib.replace_CRs input in
    require_equal [%here] (module String) input (Scanf.unescaped output);
    printf " input: %s\noutput: %s\n" input output);
  [%expect
    {|
     input:
    output:
     input: a
    output: a
     input: CR
    output: C\082
     input: XCR
    output: XC\082
     input: CR-soon
    output: C\082-soon
     input: CR-someday
    output: C\082-someday
    |}];
  return ()
;;

let%expect_test "ml output with a CR in the input" =
  let%bind () = test_output_ml "CR" in
  [%expect {|
    let foo =
      "C\082"
    ;;
  |}];
  return ()
;;
