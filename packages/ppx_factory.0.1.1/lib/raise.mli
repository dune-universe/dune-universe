(** Functions to raise ppx errors in ppx_factory

    The [loc] argument should be the loc of the problematic node within the type declaration and not
    the [loc] argument of the generator to provide the user accurate information as to which part
    of the type declaration can't be handled.
*)

(** Raise an error with the formatted message prefixed by "ppx_factory: ". *)
val errorf : loc: Ppxlib.Location.t -> ('a, unit, string, 'b) format4 -> 'a

module Default : sig
  (** Functions to raise errors specific to [[@@deriving default]] *)

  (** Raise an error with the formatted message prefixed by "ppx_factory.default: " *)
  val errorf : loc: Ppxlib.Location.t -> ('a, unit, string, 'b) format4 -> 'a
end

module Factory : sig
  (** Functions to raise errors specific to [[@@deriving factory]] *)

  (** Raise an error with the formatted message prefixed by "ppx_factory.factory: " *)
  val errorf : loc: Ppxlib.Location.t -> ('a, unit, string, 'b) format4 -> 'a

  (** Use when trying to derive a factory for an unhandled type kind.
      The message indicates factory can only be derived from record and variant types.
  *)
  val unhandled_type_kind : loc: Ppxlib.Location.t -> string -> 'a
end
