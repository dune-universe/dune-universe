open Base
open Cohttp
open Sexplib0.Sexp_conv

module Conn = struct
  type t = {
    key: string;
    name: string;
    protocol: string;
    suffix: string;
  } [@@deriving sexp]

  let equal x y =
    String.equal x.key y.key &&
    String.equal x.name y.name &&
    String.equal x.protocol y.protocol &&
    String.equal x.suffix y.suffix

  let init = {
    key = ""; name = ""; protocol = "https"; suffix = "core.windows.net"
  }

  let parse_exn s =
    let kv = String.split ~on:';' s in
    List.fold kv ~init ~f:(fun init kv ->
        match String.index kv '=' with
        | Some i ->
          let k = String.subo kv ~len:i in
          let v = String.subo kv ~pos:(i + 1) in
          if String.equal k "AccountName" then
            { init with name = v}
          else if String.equal k "AccountKey" then
            { init with key = Base64.decode_exn v }
          else if String.equal k "DefaultEndpointsProtocol" then
            { init with protocol = v}
          else if String.equal k "EndpointSuffix" then
            { init with suffix = v}
          else
            init
        | None ->
          init)
end

let ms_version = "2019-02-02"
;;

let uri ?(path="/") ?query (conn: Conn.t) =
  let b = Buffer.create 80 in
  Buffer.add_string b conn.name;
  Buffer.add_string b ".blob.";
  Buffer.add_string b conn.suffix;
  let host = Buffer.contents b in
  let scheme = conn.protocol in
  Uri.make ~scheme ~host ~path ?query ()
;;

let sign_header_exn
    ?(content_length: int64=0L)
    ?(path="/")
    ?(query: (string * string list) list = [])
    (conn: Conn.t)
    (meth: Code.meth)
    (headers: Header.t) =
  let mac = Cryptokit.MAC.hmac_sha256 conn.key in
  let mac_header k =
    Option.iter ~f:mac#add_string (Header.get headers k);
    mac#add_char '\n'
  in
  let mac_ms_headers headers =
    let f k v a =
      if String.is_prefix ~prefix:"x-ms-" k then
        (k, v) :: a
      else
        a
    in
    let kv = Header.fold f headers [] in
    let kv = List.sort kv ~compare:(fun (x, _) (y, _) -> String.compare x y) in
    List.iter kv ~f:(fun (k, v) ->
        mac#add_string k;
        mac#add_char ':';
        mac#add_string v;
        mac#add_char '\n')
  in
  let mac_content_length headers =
    match Int64.equal content_length 0L with
    | true ->
      mac#add_char '\n';
      (match meth with
       | `PUT | `POST ->
         Header.replace headers "content-length" "0"
       | _ ->
         headers)
    | false ->
      let len = Int64.to_string content_length in
      mac#add_string len;
      mac#add_char '\n';
      Header.replace headers "content-length" len
  in

  mac#add_string (Code.string_of_method meth);
  mac#add_char '\n';

  mac_header "content-encoding";
  mac_header "content-language";
  let headers = mac_content_length headers in
  mac_header "content-md5";
  mac_header "content-type";
  mac_header "date";
  mac_header "if-modified-since";
  mac_header "if-match";
  mac_header "if-none-match";
  mac_header "if-unmodified-since";
  mac_header "range";

  let headers = Header.replace headers "x-ms-version" ms_version in
  mac_ms_headers headers;

  mac#add_char '/';
  mac#add_string conn.name;
  mac#add_string path;

  List.iter query ~f:(fun (k, v) ->
      mac#add_char '\n';
      mac#add_string k;
      mac#add_char ':';
      Option.iter ~f:mac#add_string (List.hd v));

  let sign =
    let b = Buffer.create (55 + String.length conn.name) in
    Buffer.add_string b "SharedKey ";
    Buffer.add_string b conn.name;
    Buffer.add_string b ":";
    Buffer.add_string b (Base64.encode_exn mac#result);
    Buffer.contents b
  in
  Header.add_authorization headers (`Other sign)
;;
