open! Core
open! Async

(** An [Email.t] is a list of headers along with unparsed content. [Email_content.parse]
    can be used to work with the structured content of an email. *)

type t [@@deriving compare, hash, sexp_of]

val create : headers:Headers.t -> raw_content:Email_raw_content.t -> t

val headers : t -> Headers.t
val set_headers : t -> Headers.t -> t
val modify_headers : t -> f:(Headers.t -> Headers.t) -> t

val raw_content : t -> Email_raw_content.t
val set_raw_content : t -> Email_raw_content.t -> t
val modify_raw_content : t -> f:(Email_raw_content.t -> Email_raw_content.t) -> t

(** Efficiently save [t] to disk with little additional allocation.

    [?temp_file], [?perm], [?fsync] are blindly passed to [Writer.with_file_atomic] *)
val save
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool  (** default is [false] *)
  -> ?eol_except_raw_content:Lf_or_crlf.t  (** default is [`LF] *)
  -> t
  -> string
  -> unit Deferred.t

val to_bigstring_shared : ?eol_except_raw_content:Lf_or_crlf.t -> t -> Bigstring_shared.t

(** String-builder-like module. Small-to-no memory overhead
    when unparsed. *)
val to_string_monoid : ?eol_except_raw_content:Lf_or_crlf.t -> t -> String_monoid.t

val of_string : string -> t
val to_string : ?eol_except_raw_content:Lf_or_crlf.t -> t -> string

val to_bigstring : ?eol_except_raw_content:Lf_or_crlf.t -> t -> Bigstring.t
val of_bigstring : Bigstring.t -> t
val of_bigbuffer : Bigbuffer.t -> t

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io]
  end
end
