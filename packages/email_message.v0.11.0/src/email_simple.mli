open! Core

module Mimetype : sig
  type t = string [@@deriving sexp_of]
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
  module Id : sig
    type t [@@deriving compare, sexp_of]
  end

  type t

  (** In a given email, each attachment has a unique [Id.t] that is determined by the
      email structure. *)
  val id : t -> Id.t

  (** The headers surrounding this attachment *)
  val headers : t -> Headers.t

  (** [Some email] if this is an attached message/rfc822 content *)
  val embedded_email : t -> Email.t option

  (** These are expensive operations *)
  val raw_data : t -> Bigstring_shared.t Or_error.t
  val md5 : t -> string Or_error.t

  val filename : t -> attachment_name
  val to_file : t -> string -> unit Async.Deferred.Or_error.t
end

module Content : sig
  type t = private Email.t [@@deriving sexp_of]

  val of_email : Email.t -> t

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

  (* [content] and [parts] return [None] if the email doesn't properly parse. They also
     return [None] if the message has content type "message/rfc822" *)
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

type t = Email.t [@@deriving sexp_of]

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

val map_file_attachments
  :  t
  -> f : (Attachment.t -> [`Keep | `Replace of t])
  -> t

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

module Stable : sig
  module Attachment : sig
    module Id : sig
      module V1 : sig
        type t = Attachment.Id.t [@@deriving bin_io, sexp]
      end
    end
  end

  module Content : sig
    module V1 : sig type t = Content.t [@@deriving bin_io, sexp] end
  end

  module Mimetype : sig
    module V1 : sig type t = Mimetype.t [@@deriving sexp] end
  end
end
