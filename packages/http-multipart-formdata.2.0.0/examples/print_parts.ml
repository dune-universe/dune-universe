type parse_result =
  ((Http_multipart_formdata.Part_header.t * string) list, string) result
[@@deriving show, ord]

let () =
  let body =
    String.concat "\r\n"
      [ {||}
      ; {|this is a preamble text.|}
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
      ; {|-----------------------------735323031399963166993862150--|} ] in
  let parts = Queue.create () in
  let on_part header ~part_body_stream =
    let open Lwt.Infix in
    let buf = Buffer.create 0 in
    let rec loop () =
      Lwt_stream.get part_body_stream
      >>= function
      | None -> Lwt.return_unit | Some c -> Buffer.add_char buf c ; loop ()
    in
    Lwt.bind (loop ()) (fun () ->
        Lwt.return @@ Queue.push (header, Buffer.contents buf) parts ) in
  let content_type =
    {|multipart/form-data; boundary=---------------------------735323031399963166993862150|}
  in
  Lwt_result.(
    lift (Http_multipart_formdata.parse_boundary ~content_type)
    >>= fun boundary ->
    Http_multipart_formdata.parse_parts ~boundary ~on_part
      (`Stream (Lwt_stream.of_string body))
    >|= fun () -> Queue.to_seq parts |> List.of_seq)
  |> Lwt_main.run
  |> pp_parse_result Format.std_formatter
