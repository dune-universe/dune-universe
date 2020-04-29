type cohttp_request_data = {
  meth: Cohttp.Code.meth;
  body: Cohttp_lwt.Body.t option;
  request_headers: Cohttp.Header.t;
  uri: Uri.t;
}

let make_request_data meth ?data ?params ?headers ?auth url =
  let open Request in
  let request_headers = Cohttp.Header.init () in
  let request_headers =
    Cohttp.Header.add request_headers "accept-encoding" "gzip"
  in
  let body, request_headers =
    let body, new_headers_list =
      match data with
      | Some (Form data) ->
          ( Some (Utils.data_to_body data),
            [ ("Content-Type", "application/x-www-form-urlencoded") ] )
      | Some (Json json) ->
          ( Some (Utils.json_to_body json),
            [ ("Content-Type", "application/json") ] )
      | Some (Raw s) -> (Some (Cohttp_lwt.Body.of_string s), [])
      | None -> (None, [])
    in
    (body, Cohttp.Header.add_list request_headers new_headers_list)
  in
  let request_headers =
    match auth with
    | Some (Basic (username, password)) ->
        Cohttp.Header.add_authorization request_headers
          (`Basic (username, password))
    | Some (Bearer s) ->
        Cohttp.Header.add_authorization request_headers (`Other ("Bearer " ^ s))
    | None -> request_headers
  in
  let request_headers =
    match headers with
    | Some specified_headers ->
        Cohttp.Header.add_list request_headers specified_headers
    | None -> request_headers
  in
  let uri =
    match params with
    | Some params -> Uri.(add_query_params' (Uri.of_string url) params)
    | None -> Uri.of_string url
  in
  { meth; body; request_headers; uri }

(* Taken from Cohttp_lwt.Client *)
let is_meth_chunked = function
  | `HEAD -> false
  | `GET -> false
  | `DELETE -> false
  | _ -> true

(* Taken fron Cohttp_lwt.Client.call *)
let request_of_request_data { meth; body; request_headers; uri } =
  let body = Option.value ~default:`Empty body in
  let chunked = is_meth_chunked meth in
  match chunked with
  | true ->
      Cohttp.Request.make_for_client ~headers:request_headers ~chunked meth uri
      |> Lwt.return
  | false ->
      let open Lwt in
      Cohttp_lwt.Body.length body >|= fun (body_length, _buf) ->
      Cohttp.Request.make_for_client ~headers:request_headers ~chunked
        ~body_length meth uri
