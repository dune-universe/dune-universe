open! Core_kernel
open! Bio_io

(* Fasta_record equal *)

let os_equal = Option.equal String.equal

let print_sexp s = print_endline @@ Sexp.to_string_hum ~indent:1 s

let%expect_test _ =
  Fasta_record.create ~id:"" ~desc:None ~seq:""
  |> Fasta_record.sexp_of_t |> print_sexp;
  [%expect {| ((id "") (desc ()) (seq "")) |}]

let%expect_test _ =
  Fasta_record.create ~id:"a" ~desc:None ~seq:""
  |> Fasta_record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc ()) (seq "")) |}]

let%expect_test _ =
  Fasta_record.create ~id:"a" ~desc:(Some "b") ~seq:""
  |> Fasta_record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc (b)) (seq "")) |}]

let%expect_test _ =
  Fasta_record.create ~id:"a" ~desc:None ~seq:"actg"
  |> Fasta_record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc ()) (seq actg)) |}]

let%expect_test _ =
  Fasta_record.create ~id:"a r" ~desc:(Some "b") ~seq:"actg"
  |> Fasta_record.sexp_of_t |> print_sexp;
  [%expect {| ((id "a r") (desc (b)) (seq actg)) |}]

let%expect_test _ =
  let result = Fasta_record.of_header "" in
  print_endline
  @@ Sexp.to_string_hum ~indent:1 ([%sexp_of: Fasta_record.t Or_error.t] result);
  [%expect
    {|
    (Error
     ("Caught exception"
      (src/fasta_record.ml.Exn "Header line should start with '>'.  Got: ''.")))
  |}]

let%test _ =
  String.equal (Fasta_record.of_header_exn ">apple" |> Fasta_record.id) "apple"

let%test _ =
  Fasta_record.equal
    (Or_error.ok_exn @@ Fasta_record.of_header ">")
    (Fasta_record.create ~id:"" ~desc:None ~seq:"")

let%test _ =
  Fasta_record.equal
    (Or_error.ok_exn @@ Fasta_record.of_header ">apple")
    (Fasta_record.create ~id:"apple" ~desc:None ~seq:"")

let%test _ =
  Fasta_record.equal
    (Fasta_record.of_header_exn ">apple" |> Fasta_record.with_desc None)
    (Fasta_record.create ~id:"apple" ~desc:None ~seq:"")

let%test _ =
  Fasta_record.equal
    (Fasta_record.of_header_exn ">apple"
    |> Fasta_record.with_seq "A"
    |> Fasta_record.with_desc (Some "B"))
    (Fasta_record.create ~id:"apple" ~desc:(Some "B") ~seq:"A")

let%test _ =
  Fasta_record.equal
    (Or_error.ok_exn @@ Fasta_record.of_header ">apple pie")
    (Fasta_record.create ~id:"apple" ~desc:(Some "pie") ~seq:"")

let%test _ =
  Fasta_record.equal
    (Fasta_record.of_header_exn ">APPLE pie")
    (Fasta_record.of_header_exn ">apple pie" |> Fasta_record.with_id "APPLE")

let%test _ =
  not
    ((Or_error.equal Fasta_record.equal)
       (Fasta_record.of_header ">apple pie")
       (Fasta_record.of_header ""))

let%test _ =
  not
    ((Or_error.equal Fasta_record.equal)
       (Fasta_record.of_header ">apple pie")
       (Fasta_record.of_header ">"))

let%test _ =
  not
    ((Or_error.equal Fasta_record.equal)
       (Fasta_record.of_header ">apple pie")
       (Fasta_record.of_header ">apple"))
