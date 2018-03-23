open Core_kernel
open Async_kernel
open Expect_test_helpers

open Email_address

let%test_unit "check comparison" =
  let open! Int.Replace_polymorphic_compare in
  (* Ignore prefixes *)
  [ create ~prefix:"foo" ~domain:"bar.com" "A", create ~prefix:"foo" ~domain:"bar.com" "A", true
  ; create ~prefix:"foo" ~domain:"bar.com" "A", create ~prefix:"bar" ~domain:"bar.com" "A", true
  ; create ~prefix:"foo" ~domain:"bar.com" "A", create               ~domain:"bar.com" "A", true
  ; create ~prefix:"A, B" ~domain:"X.COM" "ab", create ~prefix:"A, B" ~domain:"X.COM" "ab", true

  (* Case-insensitive domain *)
  ; create ~domain:"BAR.com" "A", create ~domain:"bar.com" "A", true
  ; create ~domain:"foo.com" "A", create ~domain:"bar.com" "A", false

  (* Case-sensitive local part *)
  ; create "A", create "a", false

  ] |> Core_kernel.List.iter ~f:(fun (l, r, equal) ->
    [%test_result: bool]
      ~expect:equal
      (Email_address.compare l r = 0))
;;

let%test_unit "check example email list" =
  let open! Int.Replace_polymorphic_compare in
  [%test_result: Email_address.t list Or_error.t]
    (list_of_string {|"A, B" <ab@x.com>, "C, D" <cd@x.com> |})
    ~expect:(Ok [
      create ~prefix:"A, B" ~domain:"X.COM" "ab";
      create ~prefix:"C, D" ~domain:"x.com" "cd";
    ]);
;;

let%test_unit "compare" =
  let equal a b =
    let result = compare (of_string_exn a) (of_string_exn b) in
    [%test_result : int] result ~expect:0
  in
  equal
    "foobar <foo@bar.com>"
    "foo@bar.com"
;;

let print_addr addr =
  let prefix = prefix addr in
  let local_part = local_part addr in
  let domain = domain addr in
  print_s [%message
    ""
      (prefix : string option)
      (local_part : string)
      (domain : Domain.t option)
  ]
;;

let%expect_test "parse one" =
  let parse str = print_addr (of_string_exn str) in
  show_raise (fun () -> parse "  ");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address"
        (error     "email > local_part: count_while1")
        (input_str "  "))) |}]
  in
  parse "local";
  let%bind () =
    [%expect {| ((prefix ()) (local_part local) (domain ())) |}]
  in
  parse "<local>";
  let%bind () =
    [%expect {| ((prefix ("")) (local_part local) (domain ())) |}]
  in
  parse " local@janestreet.com ";
  let%bind () =
    [%expect {| ((prefix ()) (local_part local) (domain (janestreet.com))) |}]
  in
  parse " <local@janestreet.com> ";
  let%bind () =
    [%expect {| ((prefix ("")) (local_part local) (domain (janestreet.com))) |}]
  in
  parse " John Doe <local> ";
  let%bind () =
    [%expect {| ((prefix ("John Doe ")) (local_part local) (domain ())) |}]
  in
  parse " John Doe <local@janestreet.com> ";
  let%bind () =
    [%expect {| ((prefix ("John Doe ")) (local_part local) (domain (janestreet.com))) |}]
  in
  parse " \"Doe, John\" <local@janestreet.com> ";
  let%bind () =
    [%expect {| ((prefix ("\"Doe, John\" ")) (local_part local) (domain (janestreet.com))) |}]
  in
  show_raise (fun () -> parse "'local@janestreet.com'");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address"
        (error     ": end_of_input")
        (input_str 'local@janestreet.com'))) |}]
  in
  show_raise (fun () -> parse "\"local@janestreet.com\"");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address"
        (error     ": end_of_input")
        (input_str "\"local@janestreet.com\""))) |}]
  in
  parse "\"local@janestreet.com\" <local@janestreet.com>";
  let%bind () =
    [%expect {|
      ((prefix ("\"local@janestreet.com\" "))
       (local_part local)
       (domain (janestreet.com))) |}]
  in
  show_raise (fun () -> parse "local@janestreet.com, local@janestreet.com");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address"
        (error ": end_of_input")
        (input_str "local@janestreet.com, local@janestreet.com"))) |}]
  in
  (* Escaping *)
  show_raise (fun () -> parse "\"\\\"Description within quotes\\\"\"<local@janestreet.com>");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address"
        (error ": end_of_input")
        (input_str "\"\\\"Description within quotes\\\"\"<local@janestreet.com>"))) |}]
  in
  show_raise (fun () -> parse "local\\@@janestreet.com");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address"
        (error     ": end_of_input")
        (input_str "local\\@@janestreet.com"))) |}]
  in
  return ()
;;

let%expect_test "parse many" =
  let parse str = List.iter (list_of_string_exn str) ~f:print_addr in
  parse "";
  let%bind () =
    [%expect {| |}]
  in
  parse " ";
  let%bind () =
    [%expect {| |}]
  in
  show_raise (fun () -> parse ",");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str ,))) |}]
  in
  show_raise (fun () -> parse ", local@janestreet.com,");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str ", local@janestreet.com,"))) |}]
  in
  show_raise (fun () -> parse "local@janestreet.com, ,local@janestreet.com,");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address(es)"
        (error ": end_of_input")
        (input_str "local@janestreet.com, ,local@janestreet.com,"))) |}]
  in
  parse " \"Doe, John\" <local@janestreet.com>,\n\t\
         \"Doe, Johnny\" <local@janestreet.com> ";
  let%bind () =
    [%expect {|
    ((prefix ("\"Doe, John\" ")) (local_part local) (domain (janestreet.com)))
    ((prefix ("\"Doe, Johnny\" ")) (local_part local) (domain (janestreet.com))) |}]
  in
  parse "x@y.com, \"a@b.com\" <\"mailto:a\"@b.com>";
  let%bind () =
    [%expect {|
    ((prefix ()) (local_part x) (domain (y.com)))
    ((prefix ("\"a@b.com\" ")) (local_part "\"mailto:a\"") (domain (b.com))) |}]
  in
  show_raise (fun () -> parse "mailnull@janestreet.com (Cron Daemon)");
  let%bind () =
    [%expect {|
    (raised (
      "Failed to parse email address(es)"
      (error     ": end_of_input")
      (input_str "mailnull@janestreet.com (Cron Daemon)"))) |}]
  in
  show_raise (fun () -> parse "a@b.com<a@b.com>");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str a@b.com<a@b.com>))) |}]
  in
  show_raise (fun () -> parse "a@b.com <a@b.com>");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str "a@b.com <a@b.com>"))) |}]
  in
  show_raise (fun () -> parse "a@@b.com");
  let%bind () =
    [%expect {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str a@@b.com))) |}]
  in
  return ()
;;
