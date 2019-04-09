open Lwt

let sock_send sock s =
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output sock in
  Lwt_io.write oc s

let header_re = Str.regexp "^\\([^:]*\\):[ \t]*\\(.*\\)$"

let status_re = Str.regexp "^\\([0-9][0-9][0-9]\\) \\(.*\\)$"

let parse_header s =
  match Str.string_match header_re s 0 with
  | false ->
      failwith "Malformed header"
  | true ->
      (String.lowercase_ascii (Str.matched_group 1 s), Str.matched_group 2 s)

let read_header ic =
  Lwt_io.read_line ic >>= function
  | "" ->
      return None
  | s ->
      return (Some (parse_header s))

let parse_status_value s =
  match Str.string_match status_re s 0 with
  | false ->
      failwith "Malformed status header"
  | true ->
      let code = int_of_string (Str.matched_group 1 s) in
      let reason = Str.matched_group 2 s in
      Http_status.of_pair (code, reason)

let read_cgi_status ic =
  read_header ic >>= function
  | Some ("status", s) ->
      return (parse_status_value s)
  | _ ->
      failwith "Malformed response (status line)"

let read_headers ic =
  let rec loop ic acc =
    read_header ic >>= function
    | None ->
        return (List.rev acc)
    | Some x ->
        loop ic (`Other x :: acc)
  in
  loop ic []

let sock_receive sock =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input sock in
  read_cgi_status ic >>= fun status ->
  read_headers ic >>= fun headers ->
  Lwt_io.read ic >>= fun body ->
  return (Response.make ~status ~headers ~body:(`String body) ())

let send_request sock req = sock_send sock (Request.to_string req)

(* Raw HTTP response *)
let receive_response sock = sock_receive sock

let request_inet ~server_name ~port req =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let finally_ () = Lwt_unix.close sock in
  catch
    (fun () ->
      Lwt_unix.gethostbyname server_name >>= fun hentry ->
      if Array.length hentry.Unix.h_addr_list <= 0 then assert false ;
      Lwt_unix.connect sock
        (Unix.ADDR_INET (hentry.Unix.h_addr_list.(0), port))
      >>= fun () ->
      send_request sock req >>= fun () ->
      receive_response sock >>= fun response ->
      finally_ () >>= fun () -> return response )
    (fun e -> finally_ () >>= fun () -> raise e)

let request_sock ~socket_filename req =
  let sock = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  let finally_ () = Lwt_unix.close sock in
  catch
    (fun () ->
      Lwt_unix.(connect sock @@ Unix.ADDR_UNIX socket_filename) >>= fun () ->
      send_request sock req >>= fun () ->
      receive_response sock >>= fun response ->
      finally_ () >>= fun () -> return response )
    (fun e -> finally_ () >>= fun () -> raise e)
