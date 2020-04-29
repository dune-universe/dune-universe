open Lwt

let resolve_location_uri ~location_uri ~reference_uri =
  match Uri.host location_uri with
  | Some _ ->
      (* Absolute *)
      location_uri
  | None ->
      (* Relative *)
      (* TODO - check complies with RFC 3986 *)
      Uri.(with_path reference_uri (path location_uri))

let process (response, body) request_function meth ~uri ?params ?data ?headers
    ?auth ~request_headers ~follow_redirects =
  let status_code =
    response |> Cohttp.Response.status |> Cohttp.Code.code_of_status
  in
  let response_headers = response |> Cohttp.Response.headers in
  match
    (status_code, follow_redirects, Cohttp.Header.get_location response_headers)
  with
  | 301, true, Some location_uri
  | 302, true, Some location_uri
  | 307, true, Some location_uri
  | 308, true, Some location_uri ->
      (* Don't change method to GET on 301 (even though it's somewhat permitted) *)
      let location_uri =
        resolve_location_uri ~location_uri ~reference_uri:uri
      in
      (*       Printf.printf "Location: %s\n" (Uri.to_string location_uri); *)
      request_function meth ?params ?data ?headers ?auth
        ?follow_redirects:(Some follow_redirects)
        (Uri.to_string location_uri)
  | 303, true, Some location_uri ->
      (* Change method to GET and lose body *)
      let location_uri =
        resolve_location_uri ~location_uri ~reference_uri:uri
      in
      request_function `GET ?params ?data:None ?headers ?auth
        ?follow_redirects:(Some follow_redirects)
        (Uri.to_string location_uri)
  | _ ->
      body |> Cohttp_lwt.Body.to_string >|= fun content ->
      let content =
        match Cohttp.Header.get response_headers "content-encoding" with
        | Some "gzip" -> (
            match Ezgzip.decompress content with
            | Ok content -> content
            | Error error ->
                Format.printf "Response: %a\n" Cohttp.Response.pp_hum response;
                Format.printf "Content: '%s'\n" content;
                failwith @@ Format.(asprintf "%a" Ezgzip.pp_gzip_error error) )
        | Some _ | None -> content
      in
      {
        Response.content;
        status_code;
        headers = response_headers;
        request = { Request.url = Uri.to_string uri; headers = request_headers };
      }
