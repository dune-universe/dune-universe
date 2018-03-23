open! Core
open Async
open Email_message
open Expect_test_helpers

open Email

let parse s =
  let parsed = of_string s in
  printf !"%{sexp:t}\n" parsed;
  let roundtripped = to_string parsed in
  printf !"Successfully roundtripped: %{sexp:bool}" (String.equal s roundtripped)
;;

let%expect_test "simple" =
  parse
    "From: foo@bar.com\n\
     To: foo@bar.com\n\
     \n\
     hello world";
  let%bind () = [%expect {|
        ((headers ((From " foo@bar.com") (To " foo@bar.com")))
         (raw_content ("hello world")))
        Successfully roundtripped: true |}]
  in
  return ()
;;

let%expect_test "no newlines" =
  parse
    "";
  let%bind () = [%expect {|
        ((headers ()) (raw_content ()))
        Successfully roundtripped: true |}]
  in
  (* Header lines should be terminated with "\n". We add the missing "\n" when we
     [to_string]. *)
  parse
    "Header: hello world";
  let%bind () = [%expect {|
        ((headers ((Header " hello world"))) (raw_content ()))
        Successfully roundtripped: false |}]
  in
  return ()
;;

let%expect_test "1 newline" =
  (* This is malformed. I could imagine having [None] for [raw_content] as well. *)
  parse
    "\n\
    ";
  let%bind () = [%expect {|
        ((headers ()) (raw_content ("")))
        Successfully roundtripped: true |}]
  in
  (* This is malformed. I could imagine having [Some ""] for [raw_content] as well. *)
  parse
    "Header: hello world\n\
    ";
  let%bind () = [%expect {|
        ((headers ((Header " hello world"))) (raw_content ()))
        Successfully roundtripped: true |}]
  in
  (* This case is weird, see below for an explanation *)
  parse
    "Header: hello world\n\
     Body";
  let%bind () = [%expect {|
        ((headers ((Header " hello world"))) (raw_content (Body)))
        Successfully roundtripped: false |}]
  in
  return ()
;;

let%expect_test "2 newlines" =
  parse
    "\n\
     \n\
    ";
  let%bind () = [%expect {|
        ((headers ()) (raw_content ("\n")))
        Successfully roundtripped: true |}]
  in
  parse
    "Header: hello world\n\
     \n\
     Body";
  let%bind () = [%expect {|
        ((headers ((Header " hello world"))) (raw_content (Body)))
        Successfully roundtripped: true |}]
  in
  return ()
;;

let%expect_test "weird headers" =
  (* Google and Exim both change to "body mode" (and adds a blank line) on the first
     line that "doesn't look like a header" for some slightly different
     interpretation of that phrase:

     - Google allows ASCII 32-57,59-126 (printable characters minus colon) in a
     header name

     - Exim allows ASCII 33-57,59-126 (printable characters minus colon minus space)
     in a header name

     RFC 5322 does not specify how to handle an invalid header line.  The following
     scenarios are possible:

     1) An invalid header (according to the above rules) is written somewhere in the
     header block.  Any following headers are incorrectly treated as the start of
     the message body.

     2) An MUA forgets to put a blank line between the header block and the body.
     As long as the first body line does not look like a header (according to the
     above rules), the "right thing" happens. *)
  (* Make sure we can handle the obsolete syntax of headers with whitespace before the
     colon. This doesn't roundtrip because we remove the whitespace before the ":"*)
  parse
    "From: foo@bar.com\n\
     Obsolete-header : hello world\n";
  let%bind () = [%expect {|
        ((headers ((From " foo@bar.com") (Obsolete-header " hello world")))
         (raw_content ()))
        Successfully roundtripped: false |}]
  in
  (* Whitespace should not be a part of a header field.  Google considers this a
     valid header.  Exim treats this as the start of the body. *)
  parse
    "From: foo@bar.com\n\
     Malformed header: hello world\n";
  let%bind () = [%expect {|
        ((headers ((From " foo@bar.com")))
         (raw_content ("Malformed header: hello world\n")))
        Successfully roundtripped: false |}]
  in
  (* RFC 5322 says that field names must contain at least 1 character, however
     Google and Exim both don't have this requirement. In addition, we get some
     messages in the wild that have broken headers like this. *)
  parse
    "From: foo@bar.com\n\
     : hello world\n";
  let%bind () = [%expect {|
        ((headers ((From " foo@bar.com") ("" " hello world"))) (raw_content ()))
        Successfully roundtripped: true |}]
  in
  return ()
;;
