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
  ((Http_multipart_formdata.part_header * string) list, string) result
[@@deriving show]

let handle_upload content_type body =
  let rec read_parts reader parts =
    Http_multipart_formdata.read reader
    |> function
    | `End -> Ok (Queue.to_seq parts |> List.of_seq)
    | `Header header ->
        let body = Cstruct.(read_body reader empty |> to_string) in
        Queue.push (header, body) parts ;
        read_parts reader parts
    | `Error e -> Error e
    | _ -> assert false
  and read_body reader body =
    Http_multipart_formdata.read reader
    |> function
    | `Body_end -> body
    | `Body buf -> read_body reader (Cstruct.append body buf)
    | `Error e -> failwith e
    | _ -> assert false
  in
  Result.bind (Http_multipart_formdata.boundary content_type) (fun boundary ->
      let reader =
        Http_multipart_formdata.reader ~read_buffer_size:10 boundary
          (`Cstruct body)
      in
      read_parts reader (Queue.create ()) )

let request_handler (_ : Unix.sockaddr) reqd =
  let request = Reqd.request reqd in
  let request_body = Reqd.request_body reqd in
  let body = ref Cstruct.empty in
  Body.schedule_read request_body ~on_eof:Fun.id ~on_read:(fun bs ~off ~len ->
      let b = Cstruct.of_bigarray ~off ~len bs in
      body := Cstruct.append !body b ) ;
  Body.close_reader request_body ;
  Lwt.async (fun () ->
      match (request.meth, request.target) with
      | `GET, "/" ->
          let headers =
            Headers.of_list
              [ ("content-length", Int.to_string (String.length upload_page))
              ; ("content-type", "text/html") ]
          in
          Reqd.respond_with_string reqd
            (Response.create ~headers `OK)
            upload_page ;
          Lwt.return_unit
      | `POST, "/upload" ->
          let content_type = Headers.get_exn request.headers "content-type" in
          let parts = handle_upload content_type !body in
          Lwt.return (show_parse_result parts)
          >|= fun s ->
          let headers =
            Headers.of_list
              [ ("content-length", Int.to_string (String.length s))
              ; ("content-type", "text/plain") ]
          in
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
