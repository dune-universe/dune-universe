(** HTTP response *)

open Printf

type body =
  [ `Stream of int option * char Lwt_stream.t (* content-length, stream *)
  | `String of string (* content-length added automatically *) ]

type t = { status: Http_status.t; headers: Http_header.t list; body: body }

let make ~status ?(headers = []) ?(body = `String "") () =
  { status; headers; body }

let status_int t = Http_status.to_int t.status

let status_string t = Http_status.to_string t.status

let add_header header t = { t with headers= header :: t.headers }

let to_debug_string ?(body_max = 1000) t =
  let headers_str =
    String.concat "; "
      (List.map (fun h -> String.trim (Http_header.to_string h)) t.headers)
  in
  sprintf "{ http_status: %d (%s); headers: [ %s]; body: \"%s\";}"
    (Http_status.to_int t.status)
    (Http_status.to_string t.status)
    headers_str
    ( match t.body with
    | `String b ->
        String.sub b 0 (min (String.length b) body_max)
    | `Stream (Some c, _) ->
        Printf.sprintf "stream of %d bytes length" c
    | `Stream _ ->
        "[stream of unknown length]" )
