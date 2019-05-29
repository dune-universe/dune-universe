(** Functions to raise ppx errors in ppx_enum

    The [loc] argument should be the loc of the problematic node within the type declaration and not
    the [loc] argument of the generator to provide the user accurate information as to which part
    of the type declaration can't be handled.
*)

(** Raise an error with the formatted message prefixed by "ppx_enum: ". *)
val errorf : loc: Ppxlib.Location.t -> ('a, unit, string, 'b) format4 -> 'a

module Enum : sig
  (** Functions to raise errors specific to [[@@deriving enum]] *)

  (** Raise an error with the formatted message prefixed by "ppx_enum.enum: " *)
  val errorf : loc: Ppxlib.Location.t -> ('a, unit, string, 'b) format4 -> 'a

  (** Use when trying to derive an enum for an unhandled type kind.
      The message indicates factory can only be derived from variant types without arguments.
  *)
  val unhandled_type_kind : loc: Ppxlib.Location.t -> string -> 'a
end
