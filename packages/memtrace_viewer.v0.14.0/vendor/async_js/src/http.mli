open! Core_kernel
open! Async_kernel
open Js_of_ocaml

module Response_type : sig
  type 'a t = 'a XmlHttpRequest.response =
    | ArrayBuffer : Typed_array.arrayBuffer Js.t Js.Opt.t t
    | Blob : #File.blob Js.t Js.Opt.t t
    | Document : Dom.element Dom.document Js.t Js.Opt.t t
    | JSON : 'a Js.Opt.t t
    | Text : Js.js_string Js.t t
    | Default : string t
end

module Post_body : sig
  type t =
    | Blob of File.blob Js.t
    | Document of Dom.element Dom.document Js.t
    | String of string
    | Form_data of Form.formData Js.t
end

module Method_with_args : sig
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

module Response : sig
  type 'response t = private
    { code : int
    ; get_header : string -> string option
    ; content : 'response
    }
end

val request
  : (response_type:'resp Response_type.t
     -> Method_with_args.t
     -> 'resp Response.t Deferred.Or_error.t)
      with_request_args

val get : ?arguments:(string * string) list -> string -> string Deferred.Or_error.t
val post : ?body:Post_body.t -> string -> string Deferred.Or_error.t
