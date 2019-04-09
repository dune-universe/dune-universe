(** SCGI request *)

open Printf
open Lwt

type t =
  { meth: Http_method.t
  ; uri: Uri.t
  ; headers: (string * string) list
  ; content: string
  ; get_params: (string * string) list
  ; post_params: (string * string) list
  }

type header =
  [ `Http_cookie
  | `Http_accept_charset
  | `Http_accept_language
  | `Http_accept_encoding
  | `Http_referer
  | `Http_accept
  | `Http_content_type
  | `Http_content_md5
  | `Http_user_agent
  | `Http_origin
  | `Http_cache_control
  | `Http_content_length
  | `Http_connection
  | `Http_host
  | `Http_authorization
  | `Http_date
  | `Http_x_forwarded_proto
  | `Http_x_forwarded_port
  | `Http_x_forwarded_for
  | `Server_name
  | `Server_port
  | `Remote_port
  | `Remote_addr
  | `Server_protocol
  | `Other of string ]

let string_of_header : header -> string = function
  | `Http_cookie ->
      "http_cookie"
  | `Http_accept_charset ->
      "http_accept_charset"
  | `Http_accept_language ->
      "http_accept_language"
  | `Http_accept_encoding ->
      "http_accept_encoding"
  | `Http_referer ->
      "http_referer"
  | `Http_accept ->
      "http_accept"
  | `Http_content_type ->
      "http_content_type"
  | `Http_content_md5 ->
      "http_content_md5"
  | `Http_user_agent ->
      "http_user_agent"
  | `Http_origin ->
      "http_origin"
  | `Http_cache_control ->
      "http_cache_control"
  | `Http_content_length ->
      "http_content_length"
  | `Http_connection ->
      "http_connection"
  | `Http_host ->
      "http_host"
  | `Http_authorization ->
      "http_authorization"
  | `Http_date ->
      "http_date"
  | `Http_x_forwarded_proto ->
      "http_x_forwarded_proto"
  | `Http_x_forwarded_port ->
      "http_x_forwarded_port"
  | `Http_x_forwarded_for ->
      "http_x_forwarded_for"
  | `Server_name ->
      "server_name"
  | `Server_port ->
      "server_port"
  | `Remote_port ->
      "remote_port"
  | `Remote_addr ->
      "remote_addr"
  | `Server_protocol ->
      "server_protocol"
  | `Other s ->
      String.lowercase_ascii s

let get_header headers header_name =
  let s = string_of_header header_name in
  List.map snd (List.find_all (fun (n, _) -> n = s) headers)

let concat_query_values l =
  List.map (fun (k, vl) -> (k, String.concat "," vl)) l

let make meth uri headers content =
  let headers =
    List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers
  in
  { meth
  ; uri
  ; headers
  ; content
  ; get_params= concat_query_values (Uri.query uri)
  ; post_params=
      ( match meth with
      | `POST
        when get_header headers `Http_content_type
             = [ "application/x-www-form-urlencoded" ] ->
          concat_query_values (Uri.query_of_encoded content)
      | _ ->
          [] )
  }

let to_debug_string t =
  let s lst =
    String.concat "; "
      (List.map (fun (n, v) -> Printf.sprintf "(\"%s\", \"%s\")" n v) lst)
  in
  sprintf
    "{ content_length: %d; meth: %s; uri: \"%s\"; headers: [ %s]; content: \
     \"%s\"; get_params: [ %s]; post_params: [ %s] }"
    (String.length t.content)
    (Http_method.to_string t.meth)
    (Uri.to_string t.uri) (s t.headers) t.content (s t.get_params)
    (s t.post_params)

let of_stream stream =
  Netstring.decode stream >>= fun decoded ->
  match Headers.of_string decoded with
  | ("CONTENT_LENGTH", content_length) :: rest -> (
      (* CONTENT_LENGTH must be first header according to spec *)
      let content_length =
        try int_of_string content_length with
        | _ ->
            failwith ("Invalid content_length: [" ^ content_length ^ "]")
      in
      (* Process the remaining headers *)
      let (scgi, request_method, uri, headers) =
        List.fold_left
          (fun (s, m, u, h) -> function
            (* Look for known headers first *)
            | ("SCGI", s) ->
                (s, m, u, h)
            | ("REQUEST_METHOD", m) ->
                (s, m, u, h)
            | ("REQUEST_URI", u) ->
                (s, m, u, h)
            (* Accumulate unknown headers *)
            | header ->
                (s, m, u, header :: h) )
          ("", "", "", []) rest
      in
      match scgi with
      | "1" ->
          (* SCGI header must be 1 according to spec *)
          Lwt_stream.nget content_length stream >>= fun chars ->
          let content =
            let b = Buffer.create content_length in
            List.iter (Buffer.add_char b) chars ;
            Buffer.contents b
          in
          let req =
            make
              (Http_method.of_string request_method)
              (Uri.of_string uri) headers content
          in
          return req
      | "" ->
          failwith "Missing SCGI header"
      | _ ->
          failwith "Unexpected SCGI header" )
  | (n, _) :: _ ->
      failwith ("Expected CONTENT_LENGTH, but got [" ^ n ^ "]")
  | [] ->
      failwith "No headers found"

let to_buffer buf x =
  let headers = Buffer.create 1000 in
  let add_header k v = bprintf headers "%s\x00%s\x00" k v in
  add_header "CONTENT_LENGTH" (string_of_int (String.length x.content)) ;
  add_header "SCGI" "1" ;
  add_header "REQUEST_METHOD" (Http_method.to_string x.meth) ;
  add_header "REQUEST_URI" (Uri.to_string x.uri) ;
  List.iter (fun (k, v) -> add_header k v) x.headers ;
  let header_string = Netstring.encode (Buffer.contents headers) in
  bprintf buf "%s%s" header_string x.content

let to_string x =
  let buf = Buffer.create 1000 in
  to_buffer buf x ; Buffer.contents buf

let content_length t = String.length t.content

let meth t = t.meth

let uri t = t.uri

let path t = Uri.path t.uri

let contents t = t.content

let param t name =
  match List.assoc_opt name t.get_params with
  | None ->
      List.assoc_opt name t.post_params
  | r ->
      r

let param_exn ?default t name =
  match param t name with
  | Some x ->
      x
  | None -> (
    match default with
    | Some x ->
        x
    | None ->
        raise Not_found )

let params_get t = t.get_params

let params_post t = t.post_params

let header t name = get_header t.headers name

let cookie t (name : string) : string option =
  match get_header t.headers `Http_cookie with
  | [] ->
      None
  | cookies :: _ -> (
    try
      let split_cookies = Str.split (Str.regexp "; ") cookies in
      let cookie_pairs =
        List.map
          (fun cookie ->
            match Str.split (Str.regexp "=") cookie with
            | [ k; v ] ->
                (k, v)
            | _ ->
                raise Not_found )
          split_cookies
      in
      Some (List.assoc name cookie_pairs)
    with
    | Not_found ->
        None )
