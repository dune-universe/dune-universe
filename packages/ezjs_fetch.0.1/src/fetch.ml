open Js_of_ocaml
open Js
open Ezjs_min
type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b

let optdef f = function
  | Some x -> def (f x)
  | None -> undefined

let to_listf f a = List.map f @@ Array.to_list @@ to_array a

class type ['a] next = object
  method done_ : bool t prop
  method value : 'a optdef prop
end

class type ['a] iterator = object
  method next : 'a next t meth
end

class type ['a] array_from = object
  method from : 'a iterator t -> 'a js_array t meth
end

let array_from (it : 'a iterator t) =
  let arr : 'a array_from t = Unsafe.variable "Array" in
  arr##from it

class type headers = object
  method append : js_string t -> js_string t -> unit meth
  method delete : js_string t -> unit meth
  method entries : js_string t js_array t iterator t meth
  method forEach : (js_string t -> js_string t -> unit) callback -> unit meth
  method get : js_string t -> js_string t opt meth
  method has : js_string t -> bool t meth
  method keys : js_string t iterator t meth
  method set : js_string t -> js_string t -> unit meth
  method values : js_string t iterator t meth
end

type 'a promise = 'a Promise.promise t

class type body = object
  method body : File.file_any readonly_prop
  method bodyUsed : bool t readonly_prop
  method arrayBuffer : Typed_array.arrayBuffer t opt promise meth
  method blob : File.blob t opt promise meth
  method formData : Form.formData t opt promise meth
  method json : Unsafe.any opt promise meth
  method text : js_string t opt promise meth
end

class type request_init = object
  method cache : js_string t optdef prop
  method credentials : js_string t optdef prop
  method headers : headers t optdef prop
  method integrity : js_string t optdef prop
  method method_ : js_string t optdef prop
  method mode : js_string t optdef prop
  method redirect : js_string t optdef prop
  method referrer : js_string t optdef prop
  method body_blob : File.blob t optdef prop
  method body_string : js_string t optdef prop
  method body_buffer : Typed_array.arrayBuffer t optdef prop
  method body_formdata : Form.formData t optdef prop
  method body_urlparam : headers t optdef prop
end

class type abort_signal = object
  inherit Dom_html.eventTarget
  method aborted : bool t readonly_prop
  method abort : Dom_html.event t prop
end

class type fetch_init = object
  inherit request_init
  method referrerPolicy : js_string t optdef readonly_prop
  method keepalive : bool t optdef readonly_prop
  method signal : abort_signal t optdef readonly_prop
end

class type request = object
  inherit body
  method cache : js_string t readonly_prop
  method credentials : js_string t readonly_prop
  method destination : js_string t readonly_prop
  method headers : headers t readonly_prop
  method integrity : js_string t readonly_prop
  method method_ : js_string t readonly_prop
  method mode : js_string t readonly_prop
  method redirect : js_string t readonly_prop
  method referrer : js_string t readonly_prop
  method referrerPolicy : js_string t readonly_prop
  method url : js_string t readonly_prop
  method clone : request t meth
end

class type response_js = object
  inherit body
  method headers : headers t readonly_prop
  method ok : bool t readonly_prop
  method redirected : bool t readonly_prop
  method status : int readonly_prop
  method statusText : js_string t readonly_prop
  method trailers : headers t promise readonly_prop
  method type_ : js_string t readonly_prop
  method url : js_string t readonly_prop
  method clone : response_js t meth
  method error : response_js t meth
  method redirect : js_string t -> int optdef -> response_js t meth
end

class type global_scope = object
  method fetch : js_string t -> fetch_init t optdef -> response_js t promise meth
  method fetch_request : request t -> response_js t promise meth
end

let request_js : (js_string t -> request_init t optdef -> request t) constr =
  Unsafe.variable "Request"
let header_js : headers t constr =
  Unsafe.variable "Headers"

let global_scope : global_scope t ref = ref (Unsafe.variable "window")
let init_worker () = global_scope := Unsafe.variable "self"


let make_headers l =
  let h = new%js header_js in
  List.iter (fun (name, value) -> h##append (string name) (string value)) l;
  h

let get_headers (h : headers t)=
  let a = array_from h##entries in
  let l = to_listf (fun a -> match Array.to_list (to_array a) with
      | [ k; v] -> Some (to_string k, to_string v)
      | _ -> None) a in
  List.rev @@ List.fold_left
    (fun acc x -> match x with None -> acc | Some x -> x :: acc) [] l

type request_body =
  | RBlob of File.blob t
  | RString of string
  | RBuffer of Typed_array.arrayBuffer t
  | RFormData of Form.formData t
  | RUrlParam of (string * string) list

let request_init ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect
    ?referrer ?body () =
  match cache, credentials, headers, integrity, meth, mode, redirect, referrer, body with
  | None, None, None, None, None, None, None, None, None -> undefined
  | _ -> let r = Unsafe.obj [||] in
    r##.cache := optdef string cache;
    r##.credentials := optdef string credentials;
    r##.headers := optdef make_headers headers;
    r##.integrity := optdef string integrity;
    r##.method_ := optdef string meth;
    r##.mode := optdef string mode;
    r##.redirect := optdef string redirect;
    r##.referrer := optdef string referrer;
    (match body with
     | Some (RBlob b) -> r##.body_blob := def b
     | Some (RString s) -> r##.body_string := def (string s)
     | Some (RBuffer b) -> r##.body_buffer := def b
     | Some (RFormData f) -> r##.body_formdata := def f
     | Some (RUrlParam p) -> r##.body_urlparam := def (make_headers p)
     | _ -> ());
    def r

let request ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect
    ?referrer ?body url =
  let options = request_init ?cache ?credentials ?headers ?integrity ?meth
      ?mode ?redirect ?referrer ?body () in
  new%js request_js (string url) options

let fetch_init ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect
    ?referrer ?body ?referrerPolicy ?keepalive () : fetch_init t optdef =
  match Optdef.to_option (request_init ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect
                            ?referrer ?body ()), referrerPolicy, keepalive with
  | None, None, None -> undefined
  | r, _, _ ->
    let r = match r with
      | None -> Unsafe.obj [||]
      | Some r -> r in
    r##.referrerPolicy := optdef string referrerPolicy;
    r##.keepalive := optdef bool keepalive;
    def r

let fetch_base ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect ?referrer ?body
    ?referrerPolicy ?keepalive url =
  let options = fetch_init ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect
      ?referrer ?body ?referrerPolicy ?keepalive () in
  !global_scope##fetch (string url) options

let fetch_request_base r = !global_scope##fetch_request r

type 'a response = {
  headers : (string * string) list;
  ok : bool;
  redirected: bool;
  status: int;
  status_text: string;
  typ: string;
  url: string;
  body_used: bool;
  body: 'a;
}

let (>>=?) x f = Lwt.bind x @@ function
  | Error _ as err -> Lwt.return err
  | Ok x -> f x

let (>|=?) x f = Lwt.bind x @@ function
  | Error _ as err -> Lwt.return err
  | Ok x -> Lwt.return (Ok (f x))

let to_lwt_str ?(msg="Cannot parse body") p = Lwt.bind (Promise_lwt.to_lwt p) @@ function
  | Error _ -> Lwt.return @@ Error msg
  | Ok x -> Lwt.return @@ Ok x

let to_lwt_opt ?(msg="Cannot parse body") f p =
  Lwt.bind (to_lwt_str ~msg p) @@ function
  | Error _ as err -> Lwt.return err
  | Ok x -> match Opt.to_option x with
    | None -> Lwt.return @@ Error msg
    | Some x -> Lwt.return @@ Ok (f x)

let to_array_buffer b = to_lwt_opt (fun x -> x) b##arrayBuffer
let to_blob b = to_lwt_opt (fun x -> x) b##blob
let to_form_data b = to_lwt_opt (fun x -> x) b##formData
let to_js b = to_lwt_opt (fun x -> Unsafe.coerce x) b##json
let to_text b = to_lwt_opt to_string b##text

let to_response f (r :response_js t) =
  f r >|=? fun body -> {
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
    ?referrerPolicy ?keepalive url f =
  to_lwt_str @@ fetch_base ?cache ?credentials ?headers ?integrity ?meth ?mode ?redirect ?referrer ?body
    ?referrerPolicy ?keepalive url >>=? fun r ->
  to_response f r

let fetch_request r f =
  to_lwt_str @@ fetch_request_base r >>=? fun r ->
  to_response f r
