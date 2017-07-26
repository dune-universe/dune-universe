open! Core

type t [@@deriving sexp, bin_io, compare, hash]

val empty : unit -> t

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
val of_bigbuffer : Bigbuffer.t -> t Or_error.t

type email = t

module Multipart : sig
  type t =
    { boundary : Boundary.t
    ; prologue : Bigstring_shared.t option
    ; epilogue : Bigstring_shared.t option
    ; parts    : email list
    }
end

module Content : sig
  type t =
    | Multipart of Multipart.t
    | Data of Octet_stream.t
  [@@deriving sexp_of]
end

val headers     : t -> Headers.t

val last_header : ?whitespace:Headers.Whitespace.t -> t -> Headers.Name.t -> Headers.Value.t option
val find_all_headers : ?whitespace:Headers.Whitespace.t -> t -> Headers.Name.t -> Headers.Value.t list

val set_headers : t -> Headers.t -> t

val modify_headers : t -> f:(Headers.t -> Headers.t) -> t

val add_header : ?whitespace:Headers.Whitespace.t -> t -> name:Headers.Name.t -> value:Headers.Value.t -> t
val add_headers : ?whitespace:Headers.Whitespace.t -> t -> (Headers.Name.t * Headers.Value.t) list -> t
val set_header : ?whitespace:Headers.Whitespace.t -> t -> name:Headers.Name.t -> value:Headers.Value.t -> t

val add_header_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> name:Headers.Name.t -> value:Headers.Value.t -> t
val add_headers_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> (Headers.Name.t * Headers.Value.t) list -> t
val set_header_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> name:Headers.Name.t -> value:Headers.Value.t -> t

val filter_headers : ?whitespace:Headers.Whitespace.t -> t -> f:(name:Headers.Name.t -> value:Headers.Value.t -> bool) -> t
val map_headers : ?whitespace:Headers.Whitespace.t -> t -> f:(name:Headers.Name.t -> value:Headers.Value.t -> Headers.Value.t) -> t

val content     : t -> Content.t
val set_content : t -> Content.t -> t

(** Always sets [line_break = true] *)
val create
  :  headers : Headers.t
  -> content : Content.t
  -> t

(** Allow changing the message content to mask the actual data but retain the
    structure *)
val map_data : t -> f:(Octet_stream.t -> Octet_stream.t) -> t

(** The content of the body itself, without headers. *)
val raw_content : t -> Bigstring_shared.t

val to_bigstring_shared : t -> Bigstring_shared.t

(** String-builder-like module. Small-to-no memory overhead
    when unparsed. *)
include String_monoidable.S with type t := t

include Stringable.S    with type t := t
val to_bigstring : t -> Bigstring.t
val of_bigstring : Bigstring.t -> t Or_error.t
include Sexpable.S      with type t := t
include Comparable.S    with type t := t
include Binable.S       with type t := t

module Simple : sig
  module Mimetype : sig
    type t = string
    val text : t
    val html : t
    val pdf : t
    val jpg : t
    val png : t

    val multipart_mixed : t
    val multipart_related : t
    val multipart_alternative : t

    val from_filename : string -> t
    val from_extension : string -> t

    val guess_encoding : t -> Octet_stream.Encoding.known
  end

  type attachment_name = string

  (* For parsing attachments. Use [create ~attachments] to add attachments.
     Convenience functions for email parts that have "Content-Disposition: attachment" *)
  module Attachment : sig
    type t

    val filename : t -> attachment_name
    val email : t -> email

    (* These are expensive operations *)
    val raw_data : t -> Bigstring_shared.t Or_error.t
    val md5 : t -> string Or_error.t

    val to_file : t -> string -> unit Async.Deferred.Or_error.t
  end

  module Content : sig
    type t = private email [@@deriving bin_io, sexp_of]

    val of_email : email -> t

    val create
      :  content_type:Mimetype.t
      -> ?encoding:(Octet_stream.Encoding.known)
      -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> string
      -> t

    val html
      :  ?encoding : Octet_stream.Encoding.known (* default: `Quoted_printable *)
      -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> string
      -> t

    val text
      :  ?encoding : Octet_stream.Encoding.known (* default: `Quoted_printable *)
      -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> string
      -> t

    val of_file
      :  ?content_type:Mimetype.t
      -> ?encoding:(Octet_stream.Encoding.known)
      -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> string
      -> t Async.Deferred.t

    (* Combine 2 or more contents as alternative versions.
       List should be sorted from worst to best. *)
    val alternatives
      :  ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> t list
      -> t

    (* Combine 2 or more contents that should be bundled together *)
    val mixed
      :  ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> t list
      -> t

    (* Add related resources (e.g. inline images).
       You can reference them using 'cid:${attachment_name}' in the content.
       To attach files you should use [create ~attachments] *)
    val with_related
      :  ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> resources:(attachment_name * t) list
      -> t
      -> t

    val content_type : t -> Mimetype.t

    (* The Content-ID of the content *)
    val related_part_cid : t -> attachment_name option

    val all_related_parts : t -> (attachment_name * t) list
    val find_related : t -> attachment_name -> t option

    val all_attachments : t -> Attachment.t list
    val find_attachment : t -> attachment_name -> Attachment.t option

    val content : t -> Octet_stream.t option
    val parts : t -> t list option

    (* Get the alternative versions available. If the message is not of content type
       "multipart/alternative" then return a singleton list. *)
    val alternative_parts : t -> t list

    (* Get the 'inline' parts, This expands "Content-Type: multipart/{mixed,related}",
       stripping out any attachment parts. multipart/alternative is not expanded *)
    val inline_parts : t -> t list

    (* Save content to disk *)
    val to_file : t -> string -> unit Async.Deferred.Or_error.t
  end

  type t = email [@@deriving sexp_of]

  val create
    :  ?from:Email_address.t (* defaults to <user@host> *)
    -> to_:Email_address.t list
    -> ?cc:Email_address.t list
    -> ?reply_to:Email_address.t list
    -> subject:string
    -> ?id:string
    -> ?in_reply_to:string
    -> ?date:Time.t
    -> ?auto_generated:unit
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> ?attachments:(attachment_name * Content.t) list
    -> Content.t
    -> t

  val from : t -> Email_address.t option
  val to_ : t -> Email_address.t list option
  val cc : t -> Email_address.t list option
  val subject : t -> string option
  val id : t -> string option

  val all_attachments : t -> Attachment.t list
  val find_attachment : t -> attachment_name -> Attachment.t option
  val all_related_parts : t -> (attachment_name * Content.t) list
  val find_related : t -> attachment_name -> Content.t option

  val inline_parts : t -> Content.t list

  val map_attachments
    :  t
    -> f : (Attachment.t -> t Async.Deferred.t)
    -> t Async.Deferred.t

  module Expert : sig
    val create_raw
      :  ?from:string (* defaults to <user@host> *)
      -> to_:string list
      -> ?cc:string list
      -> ?reply_to:string list
      -> subject:string
      -> ?id:string
      -> ?in_reply_to:string
      -> ?date:string
      -> ?auto_generated:unit
      -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> ?attachments:(attachment_name * Content.t) list
      -> Content.t
      -> t

    val content
      :  whitespace:Headers.Whitespace.t
      -> extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> encoding:Octet_stream.Encoding.known
      -> string
      -> t

    val multipart
      :  whitespace:Headers.Whitespace.t
      -> content_type:Mimetype.t
      -> extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> t list
      -> t
  end
end
