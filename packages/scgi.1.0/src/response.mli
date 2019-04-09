(** SCGI response *)
type body =
  [ `Stream of int option * char Lwt_stream.t (* content-length, stream *)
  | `String of string (* content-length added automatically *) ]

type t = { status: Http_status.t; headers: Http_header.t list; body: body }

val make :
     status:Http_status.t
  -> ?headers:Http_header.t list
  -> ?body:body
  -> unit
  -> t

val status_int : t -> int

val status_string : t -> string

val add_header : Http_header.t -> t -> t

val to_debug_string : ?body_max:int -> t -> string
