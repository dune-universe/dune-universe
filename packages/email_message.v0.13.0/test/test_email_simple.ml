open! Core
open Async
open Email_message
open Expect_test_helpers
open Email.Simple

let%expect_test "[Expert.content]" =
  let content ~normalize_headers ~encoding ~extra_headers str =
    let result =
      Expert.content ~normalize_headers ~encoding ~extra_headers str |> Email.to_string
    in
    printf "%s" result
  in
  content
    ~normalize_headers:`None
    ~encoding:`Quoted_printable
    ~extra_headers:[ "header1", "value1"; "header2", "value2" ]
    "x";
  let%bind () =
    [%expect
      {|
    Content-Transfer-Encoding:quoted-printable
    header1:value1
    header2:value2

    x |}]
  in
  content
    ~normalize_headers:`Whitespace
    ~encoding:`Quoted_printable
    ~extra_headers:[]
    "x\n";
  let%bind () = [%expect {|
    Content-Transfer-Encoding: quoted-printable

    x |}] in
  return ()
;;

let replacement = (Content.text_utf8 "<REPLACED>" :> Email.t)

let parse_attachments s =
  let email = Email.of_string s in
  let attachments =
    List.map (all_attachments email) ~f:(fun attachment ->
      let id = Attachment.id attachment in
      let raw_data =
        Attachment.raw_data attachment |> ok_exn |> Bigstring_shared.to_string
      in
      id, raw_data)
  in
  let stripped = map_file_attachments email ~f:(fun _ -> `Replace replacement) in
  printf
    !"%s"
    (Sexp.to_string_hum
       [%message "" (attachments : (Attachment.Id.t * string) list) (stripped : Email.t)])
;;

let parse_attachments' l = parse_attachments (String.concat l ~sep:"\n")

let%expect_test "[all_attachments] and [map_attachments]" =
  parse_attachments
    "Content-Type: multipart/mixed; boundary=BOUNDARY1\n\n\
     --BOUNDARY1\n\
     Content-Type: multipart/alternative; boundary=BOUNDARY2\n\n\
     --BOUNDARY2\n\
     Content-Type: text/plain; charset=UTF-8\n\n\
     Simple body\n\n\
     --BOUNDARY2\n\
     Content-Type: text/html; charset=UTF-8\n\n\
     <div>Simple body</div>\n\n\
     --BOUNDARY2--\n\
     --BOUNDARY1\n\
     Content-Type: text/plain; charset=US-ASCII; name=\"attachment.txt\"\n\
     Content-Disposition: attachment; filename=\"attachment.txt\"\n\
     Content-Transfer-Encoding: base64\n\n\
     Zm9v\n\
     --BOUNDARY1--";
  let%bind () =
    [%expect
      {|
        ((attachments ((((filename attachment.txt) (path (1))) foo)))
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
             \nContent-Type: text/plain; charset=\"UTF-8\"\
             \n\
             \n<REPLACED>\
             \n--BOUNDARY1--"))))) |}]
  in
  (* - parse into "multipart/digest" parts
     - the "message/rfc822" content type is optional *)
  parse_attachments
    "Content-Type: multipart/digest; boundary=BOUNDARY\n\n\
     --BOUNDARY\n\n\
     Content-Type: multipart/mixed; boundary=BOUNDARY1\n\n\
     --BOUNDARY1\n\
     Content-Type: multipart/alternative; boundary=BOUNDARY2\n\n\
     --BOUNDARY2\n\
     Content-Type: text/plain; charset=UTF-8\n\n\
     Simple body\n\n\
     --BOUNDARY2\n\
     Content-Type: text/html; charset=UTF-8\n\n\
     <div>Simple body</div>\n\n\
     --BOUNDARY2--\n\
     --BOUNDARY1\n\
     Content-Type: text/plain; charset=US-ASCII; name=\"attachment.txt\"\n\
     Content-Disposition: attachment; filename=\"attachment.txt\"\n\
     Content-Transfer-Encoding: base64\n\n\
     Zm9v\n\
     --BOUNDARY1--\n\
     --BOUNDARY\n\
     Content-Type:message/rfc822\n\n\
     Subject: No content-Type in the message headers\n\n\n\
     --BOUNDARY--";
  let%bind () =
    [%expect
      {|
        ((attachments ((((filename attachment.txt) (path (0 0 1))) foo)))
         (stripped
          ((headers
            ((Content-Type
              " multipart/digest; boundary=\"--==::BOUNDARY::000000::==--\"")))
           (raw_content
            ( "----==::BOUNDARY::000000::==--\
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
             \nContent-Type: text/plain; charset=\"UTF-8\"\
             \n\
             \n<REPLACED>\
             \n--BOUNDARY1--\
             \n----==::BOUNDARY::000000::==--\
             \nContent-Type:message/rfc822\
             \n\
             \nSubject: No content-Type in the message headers\
             \n\
             \n\
             \n----==::BOUNDARY::000000::==----"))))) |}]
  in
  (* Look into message/rfc822 for attachments *)
  parse_attachments
    "Content-Type: multipart/mixed; boundary=\"BOUNDARY1\"\n\n\
     --BOUNDARY1\n\n\
     Testing\n\n\
     --BOUNDARY1\n\
     Content-Type: message/rfc822\n\n\
     Subject: We should parse into this message for attachments\n\
     Content-Type: multipart/mixed; boundary=\"BOUNDARY2\"\n\n\
     --BOUNDARY2\n\
     Content-Type: text/x-python; charset=US-ASCII; name=\"script.py\"\n\
     Content-Disposition: attachment; filename=\"script.py\"\n\
     Content-Transfer-Encoding: base64\n\n\
     VGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk\n\
     --BOUNDARY2--\n\n\
     --BOUNDARY1\n\
     Content-Type: message/rfc822\n\
     Content-Disposition: attachment\n\n\
     Content-Type: multipart/mixed; boundary=\"BOUNDARY3\"\n\n\
     --BOUNDARY3\n\
     Content-Type: text/x-python; charset=US-ASCII; name=\"script2.py\"\n\
     Content-Disposition: attachment; filename=\"script2.py\"\n\
     Content-Transfer-Encoding: base64\n\n\
     VGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk\n\
     --BOUNDARY3--\n\n\
     --BOUNDARY1--";
  let%bind () =
    [%expect
      {|
    ((attachments
      ((((filename script.py) (path (1 0 0))) "This attachment is blacklisted")
       (((filename unnamed-attachment) (path (2)))
         "Content-Type: multipart/mixed; boundary=\"BOUNDARY3\"\
        \n\
        \n--BOUNDARY3\
        \nContent-Type: text/x-python; charset=US-ASCII; name=\"script2.py\"\
        \nContent-Disposition: attachment; filename=\"script2.py\"\
        \nContent-Transfer-Encoding: base64\
        \n\
        \nVGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk\
        \n--BOUNDARY3--\
        \n")
       (((filename script2.py) (path (2 0 0))) "This attachment is blacklisted")))
     (stripped
      ((headers ((Content-Type " multipart/mixed; boundary=\"BOUNDARY1\"")))
       (raw_content
        ( "--BOUNDARY1\
         \n\
         \nTesting\
         \n\
         \n--BOUNDARY1\
         \nContent-Type: message/rfc822\
         \n\
         \nSubject: We should parse into this message for attachments\
         \nContent-Type: multipart/mixed; boundary=\"BOUNDARY2\"\
         \n\
         \n--BOUNDARY2\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY2--\
         \n\
         \n--BOUNDARY1\
         \nContent-Type: message/rfc822\
         \nContent-Disposition: attachment\
         \n\
         \nContent-Type: multipart/mixed; boundary=\"BOUNDARY3\"\
         \n\
         \n--BOUNDARY3\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY3--\
         \n\
         \n--BOUNDARY1--"))))) |}]
  in
  return ()
;;

let%expect_test "long attachment name" =
  parse_attachments'
    [ "Content-Type: multipart/mixed; boundary=BOUNDARY1"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: multipart/alternative; boundary=BOUNDARY2"
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/plain; charset=UTF-8"
    ; ""
    ; "Simple body"
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/html; charset=UTF-8"
    ; ""
    ; "<div>Simple body</div>"
    ; ""
    ; "--BOUNDARY2--"
    ; "--BOUNDARY1"
    ; "Content-Type: text/plain; charset=US-ASCII; name=\"attachment name"
    ; " that wraps.txt\""
    ; "Content-Disposition: attachment; filename=\"attachment name"
    ; " that wraps.txt\""
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "Zm9v"
    ; "--BOUNDARY1--"
    ];
  [%expect
    {|
    ((attachments
      ((((filename  "attachment name\
                   \nthat wraps.txt") (path (1))) foo)))
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
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1--"))))) |}]
;;
