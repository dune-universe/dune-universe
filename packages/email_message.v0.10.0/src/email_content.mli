open! Core

(** The cost depends on the encoding of the content and the main media type.

    N = Size of the message
    H = Size of the headers of the sub-message(s)

    Format: time complexity, memory complexity

    .         | 7bit, 8bit, binary | Base64, Quoted_printable
    -------------------------------------------------------------
    message   |    O(N), O(H)      | O(N), O(N)
    multipart |    O(N), O(H)      | O(N), O(N)
    other     |    O(1), O(1)      | O(N), O(N)

    Where other is any other main media type: text, image, application...

    Encoding and type can be obtained from the headers, using the modules
    Headers.Content_type and Headers.Content_transfer_encoding, and the corresponding
    default values.
*)

module Multipart : sig
  type t =
    { boundary          : Boundary.t
    ; prologue          : Bigstring_shared.t option
    ; epilogue          : Bigstring_shared.t option
    ; parts             : Email.t list
    (** [container_headers] is informational only for use when further processing parts.
        it is ignored by [to_email]. *)
    ; container_headers : Headers.t
    }
end

type t =
  | Multipart of Multipart.t
  | Message of Email.t
  | Data of Octet_stream.t
[@@deriving sexp_of]


(** [parse ?container_headers email] parses the content of [email]. The default content
    type of a multipart body changes based on the container headers.
    This only comes into play if the container had "Content-Type: multipart/digest". *)
val parse
  :  ?container_headers:Headers.t
  -> Email.t
  -> t Or_error.t

val to_email : headers:Headers.t -> t -> Email.t
val set_content : Email.t -> t -> Email.t

(** Allow changing the message content to mask the actual data but retain the
    structure *)
val map_data
  :  ?on_unparsable_content:[`Skip | `Raise]  (** default [`Skip] *)
  -> Email.t
  -> f:(Octet_stream.t -> Octet_stream.t)
  -> Email.t

val to_raw_content      : t -> Email_raw_content.t
val to_bigstring_shared : t -> Bigstring_shared.t
val to_string_monoid    : t -> String_monoid.t
