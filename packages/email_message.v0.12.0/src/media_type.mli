open! Core

type t [@@deriving sexp_of, compare]

val multipart_boundary : t -> Boundary.t option
val set_multipart_boundary : t -> Boundary.t -> t
val from_headers : Headers.t -> t option
val set_headers : Headers.t -> t -> Headers.t
val default : ?parent:t -> unit -> t

(** [create type_ subtype] represents the mime-type "type_/subtype". *)
val create : string -> string -> t

val mime_type : t -> string
val mime_subtype : t -> string
val create_multipart : string -> boundary:Boundary.t -> t
val message_rfc822 : t
val text_plain : ?charset:string -> unit -> t
val is_text : t -> bool
val is_multipart : t -> bool
val is_multipart_report : t -> bool
val is_message_rfc822 : t -> bool
val is_digest : t -> bool
