open Base
open Async
open Cohttp
open Cohttp_async
open Influxdb

let fail_if_http_error resp body =
  let code = resp |> Response.status |> Code.code_of_status in
  if Code.is_error code then
    Cohttp_async.Body.to_string body >>= fun text ->
    failwith (Printf.sprintf "HTTP error: %s" text)
  else return ()

let ping ?(port = 8086) host =
  let uri = Uri.make ~scheme:"http" ~host ~port ~path:"ping" () in
  Client.get uri >>= fun (resp, body) ->
  fail_if_http_error resp body >>= fun _ ->
  let headers = Response.headers resp in
  let build = Cohttp.Header.get headers Protocol.header_build in
  let version = Cohttp.Header.get headers Protocol.header_version in
  match Option.both build version with
  | None -> failwith "Missing ping headers"
  | Some (build, version) ->
      let open Protocol in
      return { build; version }

let write ?(precision = Precision.Nanosecond) ?(port = 8086) ~database ~host
    points =
  if List.is_empty points then failwith "No points"
  else
    let body =
      List.map ~f:(Point.to_line ~precision) points
      |> String.concat ~sep:"\n" |> Cohttp_async.Body.of_string
    in
    let uri =
      Uri.make ~scheme:"http" ~host ~port ~path:"write"
        ~query:
          [
            ("db", [ database ]);
            ("precision", [ Precision.to_string precision ]);
          ]
        ()
    in
    Client.post ~body uri >>= fun (resp, body) -> fail_if_http_error resp body
