open Core_kernel
open Async_kernel
open Js_of_ocaml
module Opt = Js.Opt
module Optdef = Js.Optdef

module Response_type = struct
  type 'a t = 'a XmlHttpRequest.response =
    | ArrayBuffer : Typed_array.arrayBuffer Js.t Opt.t t
    | Blob : #File.blob Js.t Js.Opt.t t
    | Document : Dom.element Dom.document Js.t Opt.t t
    | JSON : 'a Opt.t t
    | Text : Js.js_string Js.t t
    | Default : string t
end

module Response = struct
  type 'response t =
    { code : int
    ; get_header : string -> string option
    ; content : 'response
    }
end

module Post_body = struct
  type t =
    | Blob of File.blob Js.t
    | Document of Dom.element Dom.document Js.t
    | String of string
    | Form_data of Form.formData Js.t
end

module Method_with_args = struct
  type t =
    | Get of (string * string) list
    | Post of Post_body.t option
end

type 'k with_request_args =
  ?headers:(string * string) list
  -> ?on_progress:(loaded:int -> total:int -> unit)
  -> ?on_upload_progress:(loaded:int -> total:int -> unit)
  -> url:string
  -> 'k

let has_get_args url = Option.is_some (String.index url '?')

let request
      ?(headers = [])
      ?on_progress
      ?on_upload_progress
      ~url
      (type resp)
      ~(response_type : resp Response_type.t)
      (method_with_args : Method_with_args.t)
  =
  let url, method_string =
    match method_with_args with
    | Get args ->
      url ^ (if has_get_args url then "&" else "?") ^ Url.encode_arguments args, "GET"
    | Post _body -> url, "POST"
  in
  let req = XmlHttpRequest.create () in
  req##_open (Js.string method_string) (Js.string url) Js._true;
  (let open Response_type in
   match response_type with
   | ArrayBuffer -> req##.responseType := Js.string "arraybuffer"
   | Blob -> req##.responseType := Js.string "blob"
   | Document -> req##.responseType := Js.string "document"
   | JSON -> req##.responseType := Js.string "json"
   | Text -> req##.responseType := Js.string "text"
   | Default -> req##.responseType := Js.string "");
  List.iter headers ~f:(fun (name, value) ->
    req##setRequestHeader (Js.string name) (Js.string value));
  let response : resp Response.t Or_error.t Ivar.t = Ivar.create () in
  req##.onerror
  := Dom.handler (fun _ ->
    Ivar.fill_if_empty response (Or_error.error_string "Network error");
    Js._true);
  req##.ontimeout
  := Dom.handler (fun _ ->
    Ivar.fill_if_empty response (Or_error.error_string "Timeout");
    Js._true);
  req##.onreadystatechange
  := Js.wrap_callback (fun _ ->
    match req##.readyState with
    | DONE ->
      let res =
        if req##.status >= 200 && req##.status < 300
        then (
          let%bind.Or_error content : resp Or_error.t =
            let get_text_contents_or_error () =
              Opt.case
                req##.responseText
                (fun () ->
                   (* This case should not be entered as per the specification of
                      XMLHttpRequest at MDN web docs, because if a request is successful,
                      in state [DONE] and response_type [Text] or [Default],
                      [responseText] should not be [null].
                      See https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/responseText
                   *)
                   error_s
                     [%sexp
                       "No response returned despite successful request"
                     , { code = (req##.status : int)
                       ; status_text = (Js.to_string req##.statusText : string)
                       }])
                Result.return
            in
            let open Response_type in
            match response_type with
            | ArrayBuffer -> Ok (File.CoerceTo.arrayBuffer req##.response)
            | Blob -> Ok (File.CoerceTo.blob req##.response)
            | Document -> Ok (File.CoerceTo.document req##.response)
            | JSON -> Ok (File.CoerceTo.json req##.response)
            | Text -> get_text_contents_or_error ()
            | Default ->
              Or_error.map (get_text_contents_or_error ()) ~f:Js.to_string
          in
          let get_header s =
            Opt.case
              (req##getResponseHeader (Js.bytestring s))
              (fun () -> None)
              (fun v -> Some (Js.to_string v))
          in
          Ok { Response.content; code = req##.status; get_header })
        else
          Or_error.error_s
            [%sexp
              "Request failed"
            , { code = (req##.status : int)
              ; status_text = (Js.to_string req##.statusText : string)
              }]
      in
      Ivar.fill_if_empty response res
    | _ -> ());
  Option.iter on_progress ~f:(fun on_progress ->
    req##.onprogress
    := Dom.handler (fun e ->
      on_progress ~loaded:e##.loaded ~total:e##.total;
      Js._true));
  Optdef.iter req##.upload (fun upload ->
    Option.iter on_upload_progress ~f:(fun on_upload_progress ->
      upload##.onprogress
      := Dom.handler (fun e ->
        on_upload_progress ~loaded:e##.loaded ~total:e##.total;
        Js._true)));
  (match method_with_args with
   | Get _ -> req##send Js.null
   | Post body ->
     (match body with
      | None -> req##send Js.null
      | Some body ->
        (match body with
         | Blob b -> req##send_blob b
         | Document d -> req##send_document d
         | String s -> req##send (Js.some (Js.string s))
         | Form_data fd -> req##send_formData fd)));
  Ivar.read response
;;

let get ?(arguments = []) url =
  Deferred.Or_error.map
    ~f:(fun resp -> resp.content)
    (request ~url ~response_type:Default (Get arguments))
;;

let post ?body url =
  Deferred.Or_error.map
    ~f:(fun resp -> resp.content)
    (request ~url ~response_type:Default (Post body))
;;
