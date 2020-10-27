open Ezjs_min_lwt

include Ezjs_fetch

let (>>=?) x f = Lwt.bind x @@ function
  | Error _ as err -> Lwt.return err
  | Ok x -> f x

let (>|=?) x f = Lwt.bind x @@ function
  | Error _ as err -> Lwt.return err
  | Ok x -> Lwt.return (Ok (f x))

let catch p =
  Lwt.bind (Promise.to_lwt p) @@ function
  | Error e -> Lwt.return_error e
  | Ok r -> match Opt.to_option r with
    | None -> Lwt.return_error (error_of_string "Cannot parse response body")
    | Some x -> Lwt.return_ok x

type 'a body_translate = response_js t -> ('a, error t) result Lwt.t

let to_array_buffer : Typed_array.arrayBuffer t body_translate = fun b ->
  catch b##arrayBuffer
let to_blob : File.blob t body_translate = fun b ->
  catch b##blob
let to_form_data : Js_of_ocaml.Form.formData t body_translate = fun b ->
  catch b##formData
let to_js : 'a t body_translate = fun b ->
  catch b##json >|=? Unsafe.coerce
let to_text : string body_translate = fun b ->
  catch b##text >|=? to_string

let to_response (tr : 'a body_translate) (r :response_js t) =
  tr r >|=? fun body -> {
    headers = get_headers r##.headers;
    ok = to_bool r##.ok;
    redirected = to_bool r##.redirected;
    status = r##.status;
    status_text = to_string r##.statusText;
    typ = to_string r##.type_;
    url = to_string r##.url;
    body_used = to_bool r##.bodyUsed;
    body
  }

let fetch ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect ?referrer ?body
    ?referrerPolicy ?keepalive url tr =
  Promise.to_lwt
    (fetch_base ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect
       ?referrer ?body ?referrerPolicy ?keepalive url) >>=? fun r ->
  to_response tr r

let fetch_request r tr =
  Promise.to_lwt (fetch_request_base r) >>=? fun r ->
  to_response tr r
