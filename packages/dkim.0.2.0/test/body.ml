let () = Printexc.record_backtrace true

module Stream = Dkim.Body

let of_string x =
  let decoder = Stream.decoder () in
  let acc = ref [] in

  let rec final () =
    match Stream.decode decoder with
    | `Await -> Alcotest.failf "Unexpected await request"
    | (`CRLF | `Data _ | `Spaces _) as data ->
        Fmt.epr "Notify data.\n%!" ;
        acc := data :: !acc ;
        final ()
    | `End ->
        Fmt.epr "`End.\n%!" ;
        () in

  let rec go () =
    match Stream.decode decoder with
    | `Await ->
        Fmt.epr "Notify end of input.\n%!" ;
        Stream.src decoder Bytes.empty 0 0 ;
        final ()
    | (`CRLF | `Data _ | `Spaces _) as data ->
        Fmt.epr "Notify data.\n%!" ;
        acc := data :: !acc ;
        go ()
    | `End ->
        Fmt.epr "`End.\n%!" ;
        () in

  Stream.src decoder (Bytes.unsafe_of_string x) 0 (String.length x) ;
  go () ;
  List.rev !acc

let of_string_list l =
  let decoder = Stream.decoder () in
  let acc = ref [] in

  let rec final () =
    match Stream.decode decoder with
    | `Await -> Alcotest.failf "Unexpected await request"
    | (`CRLF | `Data _ | `Spaces _) as data ->
        Fmt.epr "Notify data.\n%!" ;
        acc := data :: !acc ;
        final ()
    | `End ->
        Fmt.epr "`End.\n%!" ;
        () in

  let rec go l =
    match Stream.decode decoder with
    | (`CRLF | `Data _ | `Spaces _) as x ->
        acc := x :: !acc ;
        go l
    | `End ->
        if List.length l = 0
        then ()
        else
          Alcotest.failf "Decoder did not consume all of the stream: %a."
            Fmt.(Dump.list string)
            l
    | `Await ->
    match l with
    | [] ->
        Stream.src decoder Bytes.empty 0 0 ;
        final ()
    | x :: r ->
        Stream.src decoder (Bytes.unsafe_of_string x) 0 (String.length x) ;
        go r in

  go l ;
  List.rev !acc

let output =
  let pp ppf = function
    | `CRLF -> Fmt.pf ppf "`CRLF"
    | `Data x -> Fmt.pf ppf "`Data %S" x
    | `Spaces x -> Fmt.pf ppf "`Spaces %S" x in
  let pp = Fmt.Dump.list pp in
  let equal a b = try List.for_all2 ( = ) a b with _ -> false in
  Alcotest.testable pp equal

let test_s0 =
  Alcotest.test_case "empty" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "") []

let test_s1 =
  Alcotest.test_case "<crlf>" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "\r\n") [ `CRLF ]

let test_s2 =
  Alcotest.test_case "foo<crlf>" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "foo\r\n") [ `Data "foo"; `CRLF ]

let test_s3 =
  Alcotest.test_case "foo<crlf>bar<crlf>" `Quick @@ fun () ->
  Alcotest.(check output)
    "[]"
    (of_string "foo\r\nbar\r\n")
    [ `Data "foo"; `CRLF; `Data "bar"; `CRLF ]

let test_s4 =
  Alcotest.test_case "foo<crlf>bar" `Quick @@ fun () ->
  Alcotest.(check output)
    "[]" (of_string "foo\r\nbar")
    [ `Data "foo"; `CRLF; `Data "bar" ]

let test_s5 =
  Alcotest.test_case "<crlf><crlf>" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "\r\n\r\n") [ `CRLF; `CRLF ]

let test_s6 =
  Alcotest.test_case "<crlf>foo" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "\r\nfoo") [ `CRLF; `Data "foo" ]

let test_s7 =
  Alcotest.test_case "\\r" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "\r") [ `Data "\r" ]

let test_s8 =
  Alcotest.test_case "\\n" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "\n") [ `Data "\n" ]

let test_s9 =
  Alcotest.test_case " " `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string " ") [ `Spaces " " ]

let test_s10 =
  Alcotest.test_case "  " `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "  ") [ `Spaces "  " ]

let test_s11 =
  Alcotest.test_case "foo  " `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "foo  ") [ `Data "foo"; `Spaces "  " ]

let test_s12 =
  Alcotest.test_case "  foo" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string "  foo") [ `Spaces "  "; `Data "foo" ]

let test_s13 =
  Alcotest.test_case "<tab><tab><crlf><tab><tab>foo<crlf>" `Quick @@ fun () ->
  Alcotest.(check output)
    "[]"
    (of_string "\t\t\r\n\t\tfoo\r\n")
    [ `Spaces "\t\t"; `CRLF; `Spaces "\t\t"; `Data "foo"; `CRLF ]

let test_l0 =
  Alcotest.test_case "empty" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string_list []) []

let test_l1 =
  Alcotest.test_case "<crlf>" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string_list [ "\r\n" ]) [ `CRLF ]

let test_l2 =
  Alcotest.test_case "<cr|lf>" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string_list [ "\r"; "\n" ]) [ `CRLF ]

let test_l3 =
  Alcotest.test_case "empty" `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string_list [ "" ]) []

let test_l4 =
  Alcotest.test_case "foo|<crlf>" `Quick @@ fun () ->
  Alcotest.(check output)
    "[]"
    (of_string_list [ "foo"; "\r\n" ])
    [ `Data "foo"; `CRLF ]

let test_l5 =
  Alcotest.test_case "foo<cr|lf>" `Quick @@ fun () ->
  Alcotest.(check output)
    "[]"
    (of_string_list [ "foo\r"; "\n" ])
    [ `Data "foo"; `CRLF ]

let test_l6 =
  Alcotest.test_case "foo|<cr|lf>" `Quick @@ fun () ->
  Alcotest.(check output)
    "[]"
    (of_string_list [ "foo"; "\r"; "\n" ])
    [ `Data "foo"; `CRLF ]

let test_l7 =
  Alcotest.test_case " | " `Quick @@ fun () ->
  Alcotest.(check output) "[]" (of_string_list [ " "; " " ]) [ `Spaces "  " ]

let test_l8 =
  Alcotest.test_case " <crlf>| " `Quick @@ fun () ->
  Alcotest.(check output)
    "[]"
    (of_string_list [ " \r\n"; " " ])
    [ `Spaces " "; `CRLF; `Spaces " " ]

let () =
  Alcotest.run "dkim"
    [
      ( "simple body canonicalization (string)",
        [
          test_s0;
          test_s1;
          test_s2;
          test_s3;
          test_s4;
          test_s5;
          test_s6;
          test_s7;
          test_s8;
          test_s9;
          test_s10;
          test_s11;
          test_s12;
          test_s13;
        ] );
      ( "simple body canonicalization (string list)",
        [
          test_l0;
          test_l1;
          test_l2;
          test_l3;
          test_l4;
          test_l5;
          test_l6;
          test_l7;
          test_l8;
        ] );
    ]
