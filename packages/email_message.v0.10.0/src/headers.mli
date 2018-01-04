open! Core

(** [Whitespace.t] specifies how to handle header values. It is used in two contexts:

    1) Transport: Specify how to turn a string into a header value. [`Normalize] will add
    the necessary for transport.

    2) Processing: Specify how to turn a header value into a string. [`Normalize] will remove
    all leading and trailing whitespace on each line in order to cleanly process the
    value. *)
module Whitespace : sig
  type t =
    [ `Raw (* Leave whitespace unchanged *)
    | `Normalize (* Cleanup leading and trailing whitespace on each line *)
    ] [@@deriving sexp_of]
  val default : t (* `Normalize *)
end

module Name : sig
  (* Case insensitive *)
  type t = string [@@deriving sexp_of, compare, hash]

  val of_string : string -> t
  val to_string : t -> string

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  (* Short hand for [let is a b = equal a (of_string b)] *)
  val is : t -> string -> bool
end

(** This is just a list of commonly used header field names for simple reuse *)
module Common : sig
  val subject : string
  val to_ : string
  val from : string
  val date : string
  val message_id : string
end

module Value : sig
  type t = string [@@deriving sexp_of, compare, hash]

  (** Normalize the whitespace for processing/
      if [whitespace == `Raw] this does nothing.
      if [whitespace = `Normalize] (default), strip leading/trailing whitespace on every line. *)
  val of_string : ?whitespace:Whitespace.t -> string -> t
  (** Normalize the whitespace for transport (insert the appropriate leading space).
      if [whitespace == `Raw] this does nothing.
      if [whitespace == `Normalize] (default), insert a leading space and indent subsequent
      lines with a tab (remove any other leading/trailing space on every line). *)
  val to_string : ?whitespace:Whitespace.t -> t -> string

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end


(* The add and set functions are same as in Field_list, except they add a space
   before the value. *)
type t [@@deriving compare, hash, sexp_of]

(** [eol] defaults to `LF *)
val to_string_monoid : ?eol:Lf_or_crlf.t -> t -> String_monoid.t
val to_string        : ?eol:Lf_or_crlf.t -> t -> string

val empty : t
val append : t -> t -> t

val of_list : whitespace:Whitespace.t -> (Name.t * Value.t) list -> t
val to_list : ?whitespace:Whitespace.t -> t -> (Name.t * Value.t) list

val last : ?whitespace:Whitespace.t -> t -> Name.t -> Value.t option
val find_all : ?whitespace:Whitespace.t -> t -> Name.t -> Value.t list
val names : t -> Name.t list

val add           : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t
val add_at_bottom : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t

val add_if_missing           : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t
val add_at_bottom_if_missing : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t

val set           : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t
val set_at_bottom : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t

val add_all           : ?whitespace:Whitespace.t -> t -> (Name.t * Value.t) list -> t
val add_all_at_bottom : ?whitespace:Whitespace.t -> t -> (Name.t * Value.t) list -> t

(** If headers with this name already exist, concatenates the values for all separated by
    a comma, and appends the new value. Otherwise, creates a new header. *)
val smash_and_add : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t

val filter : ?whitespace:Whitespace.t -> t -> f:(name:Name.t -> value:Value.t -> bool) -> t

(** rewrite header values, preserving original whitespace where possible.

    [whitespace] is used to [Value.of_string ?whitespace] the [~value] before passing to [f],
    and again to [Value.to_string ?whitespace] the result.
    If the [~value] and [f ~name ~value] are the same no change will be made (white space is preserved).

    Particularly the following is an identity transform:
    [ map ~whitespace:`Normalize ~f:(fun ~name:_ ~value -> Value.of_string ~whitespace:`Normalize value) ].
    By contrast the following will 'normalize' the whitespace on all headers.
    [ map ~whitespace:`Raw ~f:(fun ~name:_ ~value -> Value.of_string ~whitespace:`Normalize value) ].
*)
val map
  :  ?whitespace:Whitespace.t
  -> t
  -> f:(name:Name.t -> value:Value.t -> Value.t)
  -> t

val map'
  :  ?whitespace:Whitespace.t
  -> t
  -> f:(name:Name.t -> value:Value.t -> Name.t * Value.t)
  -> t

module Stable : sig
  module Name : sig
    module V1 : sig type t = Name.t [@@deriving sexp, bin_io] end
  end

  module Value : sig
    module V1 : sig type t = Value.t [@@deriving sexp, bin_io] end
  end

  module V1 : sig type nonrec t = t [@@deriving sexp, bin_io] end
end
