open Httpaf
open Httpaf_lwt_unix
open Lwt.Infix

let upload_page =
  {|<!DOCTYPE html>
    <html>
    <head>
      <title>Upload multiple files</title>
      <style>
        .main {
          margin: auto;
          padding: 20px;
          font-family: Arial, sans-serif;
          width: 600px;
          border: 1px solid gray;
        }
      </style>
    </head>
    <body>
      <div class="main">
        <form action="http://localhost:8080/upload" method="post" enctype="multipart/form-data">
          <label>Select files to upload:</label>
          <input type="file" name="name" multiple><br>
          <input type="submit" value="Upload">
        </form>
      </div>
    </body>
    </html>|}

type parse_result =
  ((Http_multipart_formdata.Part_header.t * string) list, string) result
[@@deriving show, ord]

let handle_upload content_type req_body_stream =
  let parts = Queue.create () in
  let on_part header ~part_body_stream =
    let buf = Buffer.create 0 in
    let rec loop () =
      Lwt_stream.get part_body_stream
      >>= function
      | None -> Lwt.return_unit | Some c -> Buffer.add_char buf c ; loop ()
    in
    loop ()
    >>= fun () -> Lwt.return @@ Queue.push (header, Buffer.contents buf) parts
  in
  Lwt_result.(
    lift (Http_multipart_formdata.parse_boundary ~content_type)
    >>= fun boundary ->
    Http_multipart_formdata.parse_parts ~boundary ~on_part
      (`Stream req_body_stream)
    >|= fun () -> Queue.to_seq parts |> List.of_seq)
  >|= fun parts -> show_parse_result parts

let request_handler (_ : Unix.sockaddr) reqd =
  let req_body_stream, push = Lwt_stream.create () in
  let request = Reqd.request reqd in
  let request_body = Reqd.request_body reqd in
  Body.schedule_read request_body
    ~on_eof:(fun () -> Printf.printf "on_eof\n%!" ; push None)
    ~on_read:(fun bs ~off ~len ->
      for i = 0 to len - off - 1 do
        let c = Bigstringaf.get bs i in
        push (Some c)
      done ) ;
  Body.close_reader request_body ;
  Lwt.async (fun () ->
      match (request.meth, request.target) with
      | `GET, "/" ->
          let headers =
            Headers.of_list
              [ ("content-length", Int.to_string (String.length upload_page))
              ; ("content-type", "text/html") ] in
          Reqd.respond_with_string reqd
            (Response.create ~headers `OK)
            upload_page ;
          Lwt.return_unit
      | `POST, "/upload" ->
          let content_type = Headers.get_exn request.headers "content-type" in
          handle_upload content_type req_body_stream
          >|= fun s ->
          let headers =
            Headers.of_list
              [ ("content-length", Int.to_string (String.length s))
              ; ("content-type", "text/plain") ] in
          Reqd.respond_with_string reqd (Response.create ~headers `OK) s
      | `GET, "/exit" -> Lwt.return_unit
      | _ ->
          Reqd.respond_with_string reqd
            (Response.create `Not_found)
            "Route not found" ;
          Lwt.return_unit )

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  ( match error with
  | `Exn exn ->
      Body.write_string response_body (Printexc.to_string exn) ;
      Body.write_string response_body "\n"
  | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error) ) ;
  Body.close_writer response_body

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_engine.set (new Lwt_engine.libev ()) ;
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket ~backlog:11_000 listen_address
        (Server.create_connection_handler ~request_handler ~error_handler)
      >>= fun _server -> Lwt.return_unit ) ;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  let port = ref 8080 in
  Arg.parse
    [("-p", Arg.Set_int port, " Listening port number (8080 by default)")]
    ignore "Responds to requests with a fixed string for benchmarking purposes" ;
  main !port
