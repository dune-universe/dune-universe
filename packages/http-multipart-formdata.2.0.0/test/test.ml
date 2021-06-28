type string_result = (string, string) result [@@deriving show, ord]

let%expect_test "parse_boundary" =
  let content_type =
    "multipart/form-data; \
     boundary=---------------------------735323031399963166993862150" in
  Http_multipart_formdata.parse_boundary ~content_type
  |> pp_string_result Format.std_formatter ;
  [%expect {| (Ok "---------------------------735323031399963166993862150") |}]

type parse_result =
  ((Http_multipart_formdata.Part_header.t * string) list, string) result
[@@deriving show, ord]

let%expect_test "parse_parts" =
  let body =
    String.concat "\r\n"
      [ {||}
      ; {| this is a preamble text.  |}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="text1"|}
      ; {||}
      ; {|text default|}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="text2"|}
      ; {||}
      ; {|aωb|}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file1"; filename="a.txt"|}
      ; {|Content-Type: text/plain|}
      ; {||}
      ; {|Content of a.txt.|}
      ; {||}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file2"; filename="a.html"|}
      ; {|Content-Type: text/html|}
      ; {||}
      ; {|<!DOCTYPE html><title>Content of a.html.</title>|}
      ; {||}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file3"; filename="binary"|}
      ; {|Content-Type: application/octet-stream|}
      ; {||}
      ; {|aωb|}
      ; {||}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file3"; filename="binary"; param1=value1; param2=value2|}
      ; {|Content-Type: application/octet-stream|}
      ; {||}
      ; {|aωb|}
      ; {|-----------------------------735323031399963166993862150--|} ] in
  let parts = Queue.create () in
  let on_part header ~part_body_stream =
    let open Lwt.Infix in
    let buf = Buffer.create 0 in
    let part_done, resolve_part_done = Lwt.wait () in
    Queue.push part_done parts ;
    let rec loop () =
      Lwt_stream.get part_body_stream
      >>= function
      | None -> Lwt.return_unit
      | Some c -> Buffer.add_char buf c ; (loop [@tailcall]) () in
    loop ()
    >|= fun () ->
    Lwt.wakeup_later resolve_part_done (header, Buffer.contents buf) in
  let content_type =
    {|multipart/form-data; boundary=---------------------------735323031399963166993862150|}
  in
  Lwt_result.(
    lift (Http_multipart_formdata.parse_boundary ~content_type)
    >>= fun boundary ->
    Http_multipart_formdata.parse_parts ~boundary ~on_part
      (`Stream (Lwt_stream.of_string body))
    >>= fun () -> ok (Queue.to_seq parts |> List.of_seq |> Lwt.all))
  |> Lwt_main.run
  |> fun l ->
  pp_parse_result Format.std_formatter l ;
  [%expect
    {|
    (Ok [(name: text1;
          content_type: text/plain;
          filename: ;
          parameters: , "text default");
          (name: text2;
           content_type: text/plain;
           filename: ;
           parameters: , "a\207\137b");
          (name: file1;
           content_type: text/plain;
           filename: a.txt;
           parameters: , "Content of a.txt.\r\n");
          (name: file2;
           content_type: text/html;
           filename: a.html;
           parameters: , "<!DOCTYPE html><title>Content of a.html.</title>\r\n");
          (name: file3;
           content_type: application/octet-stream;
           filename: binary;
           parameters: , "a\207\137b\r\n");
          (name: file3;
           content_type: application/octet-stream;
           filename: binary;
           parameters: (param1, value1); (param2, value2), "a\207\137b")
          ]) |}]
