module C = Config
(* Opening Httpaf shadows Config *)
open Httpaf
open Httpaf_lwt_unix

let rec serve_file config path = function
  | x :: _
    when x = Filename.current_dir_name || x = Filename.parent_dir_name ->
    None
  | x :: xs -> serve_file config (Filename.concat path x) xs
  | [] ->
    let path = Filename.concat config.C.dest_dir path in
    try
      if Sys.is_directory path then
        let path = Filename.concat path "index.html" in
        Some (Filesystem.read_bin path)
      else
        Some (Filesystem.read_bin path)
    with Sys_error _ -> None

let request_handler config _ reqd =
  let { Request.meth; target; _ } = Reqd.request reqd in
  match meth with
  | `GET ->
    let path = String.split_on_char '/' target in
    begin match serve_file config "" path with
      | Some response_body ->
        let headers =
          Headers.of_list
            [ "Content-length"
            , string_of_int (String.length response_body) ]
        in
        Reqd.respond_with_string
          reqd (Response.create ~headers `OK) response_body
      | None ->
        let headers = Headers.of_list [ "Connection", "close" ] in
        Reqd.respond_with_string
          reqd (Response.create ~headers `Not_found) ""
    end
  | _ ->
    let headers = Headers.of_list [ "Connection", "close" ] in
    Reqd.respond_with_string
      reqd (Response.create ~headers `Method_not_allowed) ""

let error_handler _ ?request:_ _ _f =
  ()

let serve_with_config config port =
  let listen_address = Unix.ADDR_INET(Unix.inet_addr_loopback, port) in
  let _server =
    Lwt_io.establish_server_with_client_socket
      listen_address
      (Server.create_connection_handler
         ~request_handler:(request_handler config)
         ~error_handler)
  in
  prerr_endline
    ("Now listening on port " ^ Int.to_string port
     ^ "... Press CTRL+C to stop.");
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let serve port = C.with_config serve_with_config port
