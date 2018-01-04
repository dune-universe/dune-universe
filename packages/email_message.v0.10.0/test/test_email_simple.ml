open! Core
open Async
open Email_message
open Expect_test_helpers

open Email.Simple

let%expect_test "[Expert.content]" =
  let content ~whitespace ~encoding ~extra_headers str =
    let result =
      Expert.content ~whitespace ~encoding ~extra_headers str
      |> Email.to_string
    in
    printf "%s" result
  in
  content
    ~whitespace:`Raw
    ~encoding:`Quoted_printable
    ~extra_headers:[ "header1", "value1"
                   ; "header2", "value2"]
    "x";
  let%bind () =
    [%expect {|
    Content-Transfer-Encoding:quoted-printable
    header1:value1
    header2:value2

    x |}]
  in
  content
    ~whitespace:`Normalize
    ~encoding:`Quoted_printable
    ~extra_headers:[]
    "x\n";
  let%bind () =
    [%expect {|
    Content-Transfer-Encoding: quoted-printable

    x |}]
  in
  return ();
;;


let replacement = (Content.text "<REPLACED>" :> Email.t)

let parse_attachments s =
  let email = Email.of_string s in
  let attachments =
    List.map (all_attachments email) ~f:(fun attachment ->
      let filename = Attachment.filename attachment in
      let raw_data =
        Attachment.raw_data attachment |> ok_exn |> Bigstring_shared.to_string
      in
      (filename, raw_data))
  in
  let%map stripped =
    map_file_attachments email ~f:(fun _ -> return (`Replace replacement))
  in
  printf !"%s"
    (Sexp.to_string_hum
       [%message "" (attachments : (string * string) list) (stripped : Email.t)])
;;

let%expect_test "[all_attachments] and [map_attachments]" =
  let%bind () =
    parse_attachments
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
       --BOUNDARY1--"
  in
  let%bind () = [%expect {|
        ((attachments ((attachment.txt foo)))
         (stripped
          ((headers ((Content-Type " multipart/mixed; boundary=BOUNDARY1")))
           (raw_content
            ( "--BOUNDARY1\
             \nContent-Type: multipart/alternative; boundary=BOUNDARY2\
             \n\
             \n--BOUNDARY2\
             \nContent-Type: text/plain; charset=UTF-8\
             \n\
             \nSimple body\
             \n\
             \n--BOUNDARY2\
             \nContent-Type: text/html; charset=UTF-8\
             \n\
             \n<div>Simple body</div>\
             \n\
             \n--BOUNDARY2--\
             \n--BOUNDARY1\
             \nContent-Transfer-Encoding: quoted-printable\
             \nContent-Type: text/plain\
             \n\
             \n<REPLACED>\
             \n--BOUNDARY1--"))))) |}]
  in
  (* - parse into "multipart/digest" parts
     - the "message/rfc822" content type is optional *)
  let%bind () =
    parse_attachments
      "Content-Type: multipart/digest; boundary=BOUNDARY\n\
       \n\
       --BOUNDARY\n\
       \n\
       Content-Type: multipart/mixed; boundary=BOUNDARY1\n\
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
       --BOUNDARY1--\n\
       --BOUNDARY\n\
       Content-Type:message/rfc822\n\
       \n\
       Subject: No content-Type in the message headers\n\
       \n\
       \n\
       --BOUNDARY--"
  in
  let%bind () = [%expect {|
        ((attachments ((attachment.txt foo)))
         (stripped
          ((headers ((Content-Type " multipart/digest; boundary=BOUNDARY")))
           (raw_content
            ( "--BOUNDARY\
             \n\
             \nContent-Type: multipart/mixed; boundary=BOUNDARY1\
             \n\
             \n--BOUNDARY1\
             \nContent-Type: multipart/alternative; boundary=BOUNDARY2\
             \n\
             \n--BOUNDARY2\
             \nContent-Type: text/plain; charset=UTF-8\
             \n\
             \nSimple body\
             \n\
             \n--BOUNDARY2\
             \nContent-Type: text/html; charset=UTF-8\
             \n\
             \n<div>Simple body</div>\
             \n\
             \n--BOUNDARY2--\
             \n--BOUNDARY1\
             \nContent-Transfer-Encoding: quoted-printable\
             \nContent-Type: text/plain\
             \n\
             \n<REPLACED>\
             \n--BOUNDARY1--\
             \n--BOUNDARY\
             \nContent-Type:message/rfc822\
             \n\
             \nSubject: No content-Type in the message headers\
             \n\
             \n\
             \n--BOUNDARY--"))))) |}]
  in
  let%bind () =
    parse_attachments
      "Content-Type: message/rfc822\n\
       Content-Disposition: attachment\n\
       \n\
       Content-Type: multipart/mixed; boundary=\"BOUNDARY\"\n\
       \n\
       --BOUNDARY\n\
       Content-Type: text/x-python; charset=US-ASCII; name=\"script.py\"\n\
       Content-Disposition: attachment; filename=\"script.py\"\n\
       Content-Transfer-Encoding: base64\n\
       \n\
       VGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk\n\
       --BOUNDARY--"
  in
  let%bind () = [%expect {|
        ((attachments
          ((unnamed-attachment
             "Content-Type: multipart/mixed; boundary=\"BOUNDARY\"\
            \n\
            \n--BOUNDARY\
            \nContent-Type: text/x-python; charset=US-ASCII; name=\"script.py\"\
            \nContent-Disposition: attachment; filename=\"script.py\"\
            \nContent-Transfer-Encoding: base64\
            \n\
            \nVGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk\
            \n--BOUNDARY--")
           (script.py "This attachment is blacklisted")))
         (stripped
          ((headers
            ((Content-Type " message/rfc822") (Content-Disposition " attachment")))
           (raw_content
            ( "Content-Type: multipart/mixed; boundary=\"BOUNDARY\"\
             \n\
             \n--BOUNDARY\
             \nContent-Transfer-Encoding: quoted-printable\
             \nContent-Type: text/plain\
             \n\
             \n<REPLACED>\
             \n--BOUNDARY--"))))) |}]
  in
  return ()
;;
