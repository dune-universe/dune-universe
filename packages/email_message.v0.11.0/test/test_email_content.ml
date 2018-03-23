open! Core
open Async
open Email_message
open Expect_test_helpers

open Email.Content

let parse s =
  let email = Email.of_string s in
  let unparsed = Email.raw_content email |> Email.Raw_content.to_bigstring_shared in
  let parsed = parse email |> ok_exn in
  print_s [%message "" ~_:(parsed : t)];
  let round_tripped = to_bigstring_shared parsed in
  assert ([%compare.equal: Bigstring_shared.t] unparsed round_tripped)
;;

let%expect_test "simple content" =
  parse
    "From: foo@bar.com\n\
     \n\
     hello world";
  let%bind () = [%expect {|
        (Data (
          (encoding Bit7)
          (content  "hello world"))) |}]
  in
  return ()
;;

let%expect_test "simple multipart" =
  parse
    "Content-Type: multipart/alternative; boundary=BOUNDARY\n\
     \n\
     --BOUNDARY\n\
     Content-Type: text/plain; charset=UTF-8\n\
     \n\
     Simple body\n\
     \n\
     --BOUNDARY\n\
     Content-Type: text/html; charset=UTF-8\n\
     Content-Transfer-Encoding: quoted-printable\n\
     \n\
     <div>Simple body</div>\n\
     \n\
     --BOUNDARY--";
  let%bind () = [%expect {|
        (Multipart (
          (boundary BOUNDARY)
          (prologue ())
          (epilogue ())
          (parts (
            ((headers ((Content-Type " text/plain; charset=UTF-8")))
             (raw_content ("Simple body\n")))
            ((headers (
               (Content-Type              " text/html; charset=UTF-8")
               (Content-Transfer-Encoding " quoted-printable")))
             (raw_content ("<div>Simple body</div>\n")))))
          (container_headers ((
            Content-Type " multipart/alternative; boundary=BOUNDARY"))))) |}]
  in
  return ()
;;

let%expect_test "nested multipart" =
  parse
    "Content-Type: multipart/mixed; boundary=BOUNDARY1\n\
     \n\
     --BOUNDARY1\n\
     Content-Type: multipart/alternative; boundary=BOUNDARY2\n\
     \n\
     --BOUNDARY2\n\
     Content-Type: text/plain; charset=UTF-8\n\
     \n\
     Simple body\n\
     \n\
     --BOUNDARY2\n\
     Content-Type: text/html; charset=UTF-8\n\
     \n\
     <div>Simple body</div>\n\
     \n\
     --BOUNDARY2--\n\
     --BOUNDARY1\n\
     Content-Type: text/plain; charset=US-ASCII; name=\"attachment.txt\"\n\
     Content-Disposition: attachment; filename=\"attachment.txt\"\n\
     Content-Transfer-Encoding: base64\n\
     \n\
     Zm9v\n\
     --BOUNDARY1--";
  let%bind () = [%expect {|
        (Multipart (
          (boundary BOUNDARY1)
          (prologue ())
          (epilogue ())
          (parts (
            ((headers ((Content-Type " multipart/alternative; boundary=BOUNDARY2")))
             (raw_content (
               "--BOUNDARY2\nContent-Type: text/plain; charset=UTF-8\n\nSimple body\n\n--BOUNDARY2\nContent-Type: text/html; charset=UTF-8\n\n<div>Simple body</div>\n\n--BOUNDARY2--")))
            ((headers (
               (Content-Type " text/plain; charset=US-ASCII; name=\"attachment.txt\"")
               (Content-Disposition " attachment; filename=\"attachment.txt\"")
               (Content-Transfer-Encoding " base64")))
             (raw_content (Zm9v)))))
          (container_headers ((Content-Type " multipart/mixed; boundary=BOUNDARY1"))))) |}]
  in
  return ()
;;

let%expect_test "message/rfc822" =
  parse
    "Content-Type: message/rfc822\n\
     \n\
     From: foo@bar.com\n\
     \n\
     Sample body";
  let%bind () = [%expect {|
        (Message ((headers ((From " foo@bar.com"))) (raw_content ("Sample body")))) |}]
  in
  return ()
;;
