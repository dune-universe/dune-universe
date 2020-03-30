(*
 * Copyright 2003-2016 Savonet team
 *
 * This file is part of Ocaml-cry.
 *
 * Ocaml-cry is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-cry is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-cry; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** OCaml low level implementation of the shout source protocol. *)

include Cry_common

let ssl = ref None
let register_https fn = ssl := Some fn

let () =
  let callback fn =
    register_https (fun ?timeout ?bind ~host sockaddr ->
        fn ?timeout ?bind ~host sockaddr)
  in
  Cry_https.register callback

let get_ssl () =
  match !ssl with Some fn -> fn | None -> raise (Error Ssl_unavailable)

let gethostbyname h =
  try Unix.gethostbyname h with Not_found -> raise (Error (Unknown_host h))

let unix_transport ?bind ?timeout sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket =
    try Unix.socket domain Unix.SOCK_STREAM 0
    with e -> raise (Error (Create e))
  in
  begin
    try
      match bind with
        | None -> ()
        | Some s ->
            let bind_addr_inet = (gethostbyname s).Unix.h_addr_list.(0) in
            (* Seems like you need to bind on port 0 *)
            let bind_addr = Unix.ADDR_INET (bind_addr_inet, 0) in
            Unix.bind socket bind_addr
    with e ->
      begin
        try Unix.close socket with _ -> ()
      end;
      raise (Error (Create e))
  end;
  let wait_for operation delay =
    let events () =
      match operation with
        | `Read -> Unix.select [socket] [] [] delay
        | `Write -> Unix.select [] [socket] [] delay
        | `Both -> Unix.select [socket] [socket] [] delay
    in
    let r, w, _ = events () in
    match operation with
      | `Read -> r <> []
      | `Write -> w <> []
      | `Both -> r <> [] || w <> []
  in
  let transport =
    {
      write = Unix.write socket;
      read = Unix.read socket;
      wait_for;
      close = (fun () -> Unix.close socket);
    }
  in
  let do_timeout = timeout <> None in
  let check_timeout () =
    match timeout with
      | Some timeout ->
          (* Block in a select call for [timeout] seconds. *)
          let _, w, _ = Unix.select [] [socket] [] timeout in
          if w = [] then raise (Error (Connect Timeout));
          Unix.clear_nonblock socket;
          transport
      | None -> assert false
  in
  let finish () =
    try
      if do_timeout then Unix.set_nonblock socket;
      Unix.connect socket sockaddr;
      if do_timeout then Unix.clear_nonblock socket;
      transport
    with
      | Unix.Unix_error (Unix.EINPROGRESS, _, _) -> check_timeout ()
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) when Sys.os_type = "Win32" ->
          check_timeout ()
      | e -> raise (Error (Connect e))
  in
  try finish ()
  with e ->
    begin
      try Unix.close socket with _ -> ()
    end;
    raise e

let rec string_of_error = function
  | Error (Create e) -> pp "could not initiate a new handler" e
  | Error (Connect e) -> pp "could not connect to host" e
  | Error (Write e) -> pp "could not write data to host" e
  | Error (Read e) -> pp "could not read data from host" e
  | Error (Close e) -> pp "could not close connection" e
  | Error Busy -> "busy"
  | Error Ssl_unavailable -> "SSL transport is not available"
  | Error Not_connected -> "not connected"
  | Error Invalid_usage -> "invalid usage"
  (*  | Unix.unix_error (code,name,param) ->
          Printf.sprintf "%s in %s(%s)" (Unix.error_message code)
                                         name param *)
  | Error (Unknown_host h) -> Printf.sprintf "Unknown host: %s" h
  | Timeout -> "connection timeout"
  | Error (Bad_answer s) ->
      Printf.sprintf "bad answer%s"
        (match s with Some s -> Printf.sprintf ": %s" s | None -> "")
  | Error (Http_answer (c, x, v)) -> Printf.sprintf "%i, %s (HTTP/%s)" c x v
  | e -> Printexc.to_string e

and pp s e = Printf.sprintf "%s: %s" s (string_of_error e)

type verb = Put | Post | Source

let string_of_verb = function
  | Put -> "PUT"
  | Post -> "POST"
  | Source -> "SOURCE"

type protocol = Icy | Http of verb | Https of verb

let string_of_protocol = function
  | Icy -> "icy"
  | Http v -> Printf.sprintf "http(%s)" (string_of_verb v)
  | Https v -> Printf.sprintf "https(%s)" (string_of_verb v)

type content_type = string

let ogg_application = "application/ogg"
let ogg_audio = "audio/ogg"
let ogg_video = "video/ogg"
let mpeg = "audio/mpeg"
let content_type_of_string s = s
let string_of_content_type x = x

type mount = Icy_id of int | Icecast_mount of string

type connection = {
  mount : mount;
  user : string;
  password : string;
  host : string;
  port : int;
  chunked : bool;
  content_type : content_type;
  protocol : protocol;
  headers : (string, string) Hashtbl.t;
}

let string_of_mount = function
  | Icy_id id -> Printf.sprintf "\"sid\": %d" id
  | Icecast_mount mount -> Printf.sprintf "\"mount\": %S" mount

let string_of_connection c =
  Printf.sprintf
    "{ %s,\n\
     \"user\":  %S,\n\
     \"password\": %S,\n\
     \"host\": %S,\n\
     \"port\": %d,\n\
     \"chunked\": %b,\n\
     \"content_type\": %S,\n\
     \"protocol\": %S,\n\
     \"headers\": { %s } }" (string_of_mount c.mount) c.user c.password c.host
    c.port c.chunked
    (string_of_content_type c.content_type)
    (string_of_protocol c.protocol)
    (Hashtbl.fold
       (fun x y z -> Printf.sprintf "%S: %S,\n%s" x y z)
       c.headers "")

type audio_info = (string, string) Hashtbl.t
type metadata = (string, string) Hashtbl.t

(* Metadata socket is only present if icy_cap is true *)
type connection_data = { connection : connection; transport : transport }
type status_priv = PrivConnected of connection_data | PrivDisconnected
type status = Connected of connection_data | Disconnected

type t = {
  timeout : float;
  connection_timeout : float option;
  bind : string option;
  mutable icy_cap : bool;
  mutable status : status_priv;
  mutable chunked : bool;
}

let get_connection_data x =
  match x.status with
    | PrivConnected d -> d
    | PrivDisconnected -> raise (Error Not_connected)

let create ?bind ?connection_timeout ?(timeout = 30.) () =
  {
    timeout;
    connection_timeout;
    bind;
    icy_cap = false;
    status = PrivDisconnected;
    chunked = false;
  }

let write_data ~timeout ?(offset = 0) ?length transport request =
  let request = Bytes.unsafe_of_string request in
  let len = match length with Some l -> l | None -> Bytes.length request in
  let rec write ofs =
    if not (transport.wait_for `Write timeout) then raise Timeout;
    let rem = len - ofs in
    if rem > 0 then (
      let ret = transport.write request ofs rem in
      if ret = 0 then raise (Failure "connection closed.");
      if ret < rem then write (ofs + ret) )
  in
  try write offset with e -> raise (Error (Write e))

let close x =
  try
    let c = get_connection_data x in
    if x.chunked then write_data ~timeout:x.timeout c.transport "0\r\n\r\n";
    c.transport.close ();
    x.chunked <- false;
    x.icy_cap <- false;
    x.status <- PrivDisconnected
  with
    | Error _ as e -> raise e
    | e -> raise (Error (Close e))

let get_status c =
  match c.status with
    | PrivConnected d -> Connected d
    | PrivDisconnected -> Disconnected

let get_icy_cap c = c.icy_cap

let audio_info ?samplerate ?channels ?quality ?bitrate () =
  let infos = Hashtbl.create 10 in
  let f m x y =
    match y with Some v -> Hashtbl.replace infos x (m v) | None -> ()
  in
  f string_of_int "channels" channels;
  f string_of_int "samplerate" samplerate;
  f string_of_int "bitrate" bitrate;
  f string_of_float "quality" quality;
  infos

let normalize_mount mount =
  match mount with
    | Icecast_mount mount ->
        Icecast_mount
          begin
            match mount with
            | "" -> "/"
            | mount ->
                if mount.[0] = '/' then mount else Printf.sprintf "/%s" mount
          end
    | _ -> mount

let connection ?user_agent ?name ?genre ?url ?public ?audio_info ?description
    ?(host = "localhost") ?(port = 8000) ?(chunked = false)
    ?(password = "hackme") ?(protocol = Http Source) ?(user = "source") ~mount
    ~content_type () =
  let headers = Hashtbl.create 10 in
  let public =
    match public with
      | Some x -> if x then Some "1" else Some "0"
      | None -> None
  in
  let mount = normalize_mount mount in
  let f (x, y) =
    match y with Some v -> Hashtbl.replace headers x v | None -> ()
  in
  let x =
    match protocol with
      | Http _ | Https _ ->
          let audio_info =
            match audio_info with
              | Some x ->
                  let f x y z =
                    let z = if z <> "" then z ^ ";" else z in
                    Printf.sprintf "%s%s=%s" z x y
                  in
                  Some (Hashtbl.fold f x "")
              | None -> None
          in
          [
            ("User-Agent", user_agent);
            ("ice-name", name);
            ("ice-genre", genre);
            ("ice-url", url);
            ("ice-public", public);
            ("ice-audio-info", audio_info);
            ("ice-description", description);
          ]
      | Icy ->
          [
            ("icy-name", name);
            ("icy-url", url);
            ("icy-pub", public);
            ("icy-genre", genre);
            ( "icy-br",
              match audio_info with
                | None -> None
                | Some x -> (
                    try Some (Hashtbl.find x "bitrate") with Not_found -> None )
            );
          ]
  in
  List.iter f x;
  {
    host;
    port;
    user;
    chunked;
    password;
    mount;
    content_type;
    protocol;
    headers;
  }

(** Base 64 encoding. *)
let encode64 s =
  let digit =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let extra = String.length s mod 3 in
  let s = match extra with 1 -> s ^ "\000\000" | 2 -> s ^ "\000" | _ -> s in
  let n = String.length s in
  let dst = Bytes.create (4 * (n / 3)) in
  for i = 0 to (n / 3) - 1 do
    let ( := ) j v = Bytes.set dst ((i * 4) + j) digit.[v] in
    let c j = int_of_char s.[(i * 3) + j] in
    let c0 = c 0 and c1 = c 1 and c2 = c 2 in
    0 := c0 lsr 2;
    1 := (c0 lsl 4) land 63 lor (c1 lsr 4);
    2 := (c1 lsl 2) land 63 lor (c2 lsr 6);
    3 := c2 land 63
  done;
  if extra = 1 then begin
    Bytes.set dst ((4 * (n / 3)) - 2) '=';
    Bytes.set dst ((4 * (n / 3)) - 1) '='
  end
  else if extra = 2 then Bytes.set dst ((4 * (n / 3)) - 1) '=';
  Bytes.to_string dst

(* URL encoding/decoging according to RFC 1738, RFC 1630.
 * Borrowed from ocamlnet. *)

(** Converts k to a 2-digit hexadecimal string. *)
let to_hex2 =
  let hex_digits =
    [|
      '0';
      '1';
      '2';
      '3';
      '4';
      '5';
      '6';
      '7';
      '8';
      '9';
      'A';
      'B';
      'C';
      'D';
      'E';
      'F';
    |]
  in
  fun k ->
    let s = Bytes.create 2 in
    Bytes.set s 0 hex_digits.((k lsr 4) land 15);
    Bytes.set s 1 hex_digits.(k land 15);
    Bytes.to_string s

let url_encode s =
  let rec do_url_encode s s' =
    (* True if x is an acceptable char *)
    let range x =
      (* 0-9 *)
      (x >= 0x30 && x <= 0x39)
      (* A-Z *)
      || (x >= 0x41 && x <= 0x5A)
      || (* a-z *)
      (x >= 0x61 && x <= 0x7a)
    in
    match String.length s with
      | 0 -> s'
      | l ->
          let x = Char.code s.[0] in
          let s' =
            if not (range x) then Printf.sprintf "%s%%%s" s' (to_hex2 x)
            else Printf.sprintf "%s%c" s' s.[0]
          in
          do_url_encode (String.sub s 1 (l - 1)) s'
  in
  do_url_encode s ""

let http_header = Printf.sprintf "%s %s HTTP/1.%d\r\n%s\r\n\r\n"

let get_auth user password =
  Printf.sprintf "Basic %s" (encode64 (user ^ ":" ^ password))

let buf = Bytes.create 1024

(* TODO: review all reading code as this 
 * seems a bit ad-hoc/naive.. *)

(** Read and split data. 
  * There should always be at least
  * one element in the resulting list.
  * If not, something bad happened. *)
let read_data ~timeout transport =
  if not (transport.wait_for `Read timeout) then raise Timeout;
  try
    let ret = transport.read buf 0 1024 in
    (* split data *)
    let buf = Bytes.sub buf 0 ret in
    let rec f pos l =
      try
        let x = Bytes.index_from buf pos '\n' in
        f (x + 1) (Bytes.sub buf pos (x - pos - 1) :: l)
      with Invalid_argument _ | Not_found ->
        if pos < ret then Bytes.sub buf pos (ret - pos) :: l else l
    in
    let ret = f 0 [] in
    if List.length ret = 0 then [Bytes.of_string ""] else List.rev (f 0 [])
  with e -> raise (Error (Read e))

let header_string headers source =
  let unique_headers = Hashtbl.create 10 in
  Hashtbl.iter (Hashtbl.replace unique_headers) headers;
  (* Icy headers are of the form: %s:%s *)
  let sep = if source.protocol = Icy then "" else " " in
  (* "content-type" seems to be in lower case
   * for shoutcast. Also, it seems that
   * it is good to pass it last, see:
   *   http://forums.winamp.com/showthread.php?threadid=285035 *)
  let content_label =
    if source.protocol = Icy then "content-type" else "Content-Type"
  in
  let f x y z = Printf.sprintf "%s:%s%s" x sep y :: z in
  let headers =
    Hashtbl.fold f unique_headers (f content_label source.content_type [])
  in
  String.concat "\r\n" headers

let parse_http_answer s =
  let f v c s = (v, c, s) in
  try Scanf.sscanf s "HTTP/%s %i %[^\r^\n]" f with
    | Scanf.Scan_failure s -> raise (Error (Bad_answer (Some s)))
    | _ -> raise (Error (Bad_answer None))

let resolve_host host = (Unix.gethostbyname host).Unix.h_addr_list.(0)

let add_host_header ?(force = false) headers host port =
  try
    ignore (resolve_host host);
    if force then Hashtbl.replace headers "Host" ""
  with Not_found ->
    let host = if port = 80 then host else Printf.sprintf "%s:%d" host port in
    Hashtbl.replace headers "Host" host

let http_path_of_mount = function
  | Icecast_mount mount -> mount
  | _ -> raise (Error Invalid_usage)

let connect_http c transport source verb =
  let auth = get_auth source.user source.password in
  try
    let http_version = if source.chunked then 1 else 0 in
    let headers = Hashtbl.copy source.headers in
    Hashtbl.replace headers "Authorization" auth;
    add_host_header ~force:(http_version = 1) headers source.host source.port;
    if source.chunked then Hashtbl.replace headers "Transfer-Encoding" "chunked";
    if verb = Put then Hashtbl.add headers "Expect" "100-continue";
    let headers = header_string headers source in
    let request =
      http_header (string_of_verb verb)
        (http_path_of_mount source.mount)
        http_version headers
    in
    write_data ~timeout:c.timeout transport request;
    (* Read input from socket. *)
    let ret = read_data ~timeout:c.timeout transport in
    let v, code, s = parse_http_answer (Bytes.to_string (List.hd ret)) in
    let err = Error (Http_answer (code, s, v)) in
    begin
      match verb with
      | Put when code <> 100 -> raise err
      | v when v <> Put && (code < 200 || code >= 300) -> raise err
      | _ -> ()
    end;
    c.chunked <- source.chunked;
    c.icy_cap <- true;
    c.status <- PrivConnected { connection = source; transport }
  with e ->
    begin
      try close c with _ -> ()
    end;
    raise e

let icy_id_of_mount = function
  | Icy_id id -> id
  | _ -> raise (Error Invalid_usage)

let connect_icy c transport source =
  let password =
    let user =
      match source.user with "" -> "" | user -> Printf.sprintf "%s:" user
    in
    let id =
      match icy_id_of_mount source.mount with
        | 1 -> ""
        | id -> Printf.sprintf ":#%d" id
    in
    Printf.sprintf "%s%s%s" user source.password id
  in
  let request = Printf.sprintf "%s\r\n" password in
  try
    write_data ~timeout:c.timeout transport request;
    (* Read input from socket. *)
    let ret = read_data ~timeout:c.timeout transport in
    let f s =
      if s.[0] <> 'O' && s.[1] <> 'K' then raise (Error (Bad_answer (Some s)))
    in
    begin
      try Scanf.sscanf (Bytes.to_string (List.hd ret)) "%[^\r^\n]" f with
      | Scanf.Scan_failure s -> raise (Error (Bad_answer (Some s)))
      | Error _ as e -> raise e
      | _ -> raise (Error (Bad_answer None))
    end;
    (* Read another line *)
    let ret =
      match ret with
        | _ :: y :: _ -> y
        | _ -> List.hd (read_data ~timeout:c.timeout transport)
    in
    let f _ = c.icy_cap <- true in
    begin
      try Scanf.sscanf (Bytes.to_string ret) "icy-caps:%[1]" f
      with Scanf.Scan_failure _ -> ()
    end;
    (* Now Write headers *)
    let headers = header_string source.headers source in
    let request = Printf.sprintf "%s\r\n\r\n" headers in
    write_data ~timeout:c.timeout transport request;
    c.status <- PrivConnected { connection = source; transport }
  with e ->
    begin
      try close c with _ -> ()
    end;
    raise e

let connect c source =
  if c.status <> PrivDisconnected then raise (Error Busy);
  let port = if source.protocol = Icy then source.port + 1 else source.port in
  let sockaddr = Unix.ADDR_INET (resolve_host source.host, port) in
  let transport =
    match source.protocol with
      | Icy | Http _ -> unix_transport ?bind:c.bind sockaddr
      | Https _ -> (get_ssl ()) ~host:source.host sockaddr
  in
  try
    (* We do not know icy capabilities so far.. *)
    c.icy_cap <- false;
    match source.protocol with
      | Http verb | Https verb -> connect_http c transport source verb
      | Icy -> connect_icy c transport source
  with e ->
    begin
      try transport.close () with _ -> ()
    end;
    raise e

let http_meta_request mount charset meta headers =
  let unique_headers = Hashtbl.create 10 in
  Hashtbl.iter (Hashtbl.replace unique_headers) headers;
  let header = Hashtbl.fold (Printf.sprintf "%s: %s\r\n%s") unique_headers "" in
  Printf.sprintf
    "GET /admin/metadata?mode=updinfo&mount=%s%s%s HTTP/1.0\r\n%s\r\n" mount
    charset meta header

let icy_meta_request =
  Printf.sprintf "GET /admin.cgi?mode=updinfo&pass=%s%s HTTP/1.0\r\n%s\r\n"

let manual_update_metadata ~host ~port ~protocol ~user ~password ~mount
    ?(connection_timeout = 5.) ?(timeout = 30.) ?headers ?bind ?charset m =
  let mount = normalize_mount mount in
  let headers =
    match headers with Some x -> Hashtbl.copy x | None -> Hashtbl.create 0
  in
  let sockaddr = Unix.ADDR_INET (resolve_host host, port) in
  let transport =
    match protocol with
      | Icy | Http _ ->
          unix_transport ?bind ~timeout:connection_timeout sockaddr
      | Https _ -> (get_ssl ()) ~timeout:connection_timeout ~host sockaddr
  in
  let close () = try transport.close () with e -> raise (Error (Close e)) in
  try
    let charset =
      match charset with Some c -> Printf.sprintf "&charset=%s" c | None -> ""
    in
    let f x y z =
      if y <> "" then Printf.sprintf "%s&%s=%s" z (url_encode x) (url_encode y)
      else z
    in
    let meta = Hashtbl.fold f m "" in
    let request =
      match protocol with
        | Http _ | Https _ ->
            let mount =
              match mount with
                | Icecast_mount mount -> mount
                | _ -> raise (Error Invalid_usage)
            in
            Hashtbl.replace headers "Authorization" (get_auth user password);
            add_host_header headers host port;
            http_meta_request mount charset meta headers
        | Icy ->
            let sid =
              match mount with
                | Icy_id id -> id
                | _ -> raise (Error Invalid_usage)
            in
            let meta =
              match sid with
                | 1 -> meta
                | sid -> Printf.sprintf "%s&sid=%d" meta sid
            in
            let user_agent =
              try Hashtbl.find headers "User-Agent"
              with Not_found -> "ocaml-cry"
            in
            let user_agent =
              Printf.sprintf "User-Agent: %s (Mozilla compatible)\r\n"
                user_agent
            in
            icy_meta_request password meta user_agent
    in
    write_data ~timeout transport request;
    (* Read input from socket. *)
    let ret = read_data ~timeout transport in
    let v, code, s = parse_http_answer (Bytes.to_string (List.hd ret)) in
    if code <> 200 then raise (Error (Http_answer (code, s, v)));
    close ()
  with e ->
    begin
      try close () with _ -> ()
    end;
    raise e

let update_metadata ?charset c m =
  if not c.icy_cap then raise (Error Invalid_usage);
  let data = get_connection_data c in
  let source = data.connection in
  let user = source.user in
  let port = source.port in
  let password = source.password in
  let headers = Some source.headers in
  let protocol = source.protocol in
  let mount = source.mount in
  let host = source.host in
  let timeout = c.timeout in
  let connection_timeout = c.connection_timeout in
  manual_update_metadata ~host ~port ~protocol ~user ~password ~timeout ~mount
    ?headers ?connection_timeout ?charset m

let send ?(offset = 0) ?length c x =
  let length = match length with Some l -> l | None -> String.length x in
  let d = get_connection_data c in
  if c.chunked then begin
    if length > 0 then (
      let x =
        Printf.sprintf "%X\r\n%s\r\n" length (String.sub x offset length)
      in
      write_data ~timeout:c.timeout d.transport x )
  end
  else write_data ~offset ~length ~timeout:c.timeout d.transport x
