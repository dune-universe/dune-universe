open Cohttp
open Cohttp_async

let rfc1123_date_string tm =
  let open Core.Unix in
  strftime (gmtime tm) "%a, %d %b %Y %H:%M:%S GMT"
;;

let sign_exn
    ?content_length
    ?(headers: Header.t=Header.init ()) ~meth ~path ?query conn =
  let uri = Azblob.uri conn ~path ?query in
  let date = rfc1123_date_string (Core.Unix.time ()) in
  let headers = Header.add headers "x-ms-date" date in
  let headers = Azblob.sign_header_exn conn meth headers ~path ?query ?content_length in
  headers, uri
;;

let body_length_exn = function
  | `Empty     as body -> Cohttp.Body.length body
  | `String _  as body -> Cohttp.Body.length body
  | `Strings _ as body -> Cohttp.Body.length body
  | _ -> failwith "Fixed length body required."
;;

let get_blob ?interrupt ?ssl_config
    ?headers ~path conn =
  let headers, uri = sign_exn conn ~meth:`GET ~path ?headers in
  Client.get ?interrupt ?ssl_config ~headers uri
;;

let delete_blob ?interrupt ?ssl_config
    ?headers ~path conn =
  let headers, uri = sign_exn conn ~meth:`DELETE ~path ?headers in
  Client.delete ?interrupt ?ssl_config ~headers uri
;;

let put_blob ?interrupt ?ssl_config
    ?(blob_type=`Block)
    ?(headers: Header.t=Header.init ())
    ~path ?(body:Cohttp_async.Body.t=`Empty) conn =
  let content_length = body_length_exn body in
  let headers =
    let blob_type =
      match blob_type with
      | `Block -> "BlockBlob"
      | `Append -> "AppendBlob"
    in
    Header.replace headers "x-ms-blob-type" blob_type
  in
  let headers, uri = sign_exn conn ~meth:`PUT ~path ~headers ~content_length in
  Client.put ?interrupt ?ssl_config ~headers uri ~body
;;

let append_block ?interrupt ?ssl_config
    ?(headers: Header.t=Header.init ())
    ~path ?(body:Cohttp_async.Body.t=`Empty) conn =
  let content_length = body_length_exn body in
  let query = [ "comp", [ "appendblock" ] ] in
  let headers, uri =
    sign_exn conn ~meth:`PUT ~headers ~path ~query ~content_length in
  Client.put ?interrupt ?ssl_config ~headers uri ~body
;;
