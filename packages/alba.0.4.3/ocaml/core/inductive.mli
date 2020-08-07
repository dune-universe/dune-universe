type params = (string * Term.typ) array

module Header:
sig
    type t

    val make:
        string
        -> Term.typ
        -> (Term.Pi_info.t * Term.typ) list
        -> Term.Sort.t
        -> t
    (** [make name kind indices sort] *)

    val name: t -> string

    val count_indices: t -> int

    val has_index: t -> bool

    val kind: params -> t -> Term.typ
    (** [kind params header] returns the kind of the inductive type described by
    [header] with the parameters prefixed. *)


    val default_type: int -> params -> t array -> Term.typ
    (** [default_type itype params headers] the default type of the [itype]th
    member of the family. *)


    val is_well_constructed:
        int -> params -> t array -> int -> Term.typ -> bool
    (** [is_well_constructed itype params headers nargs typ] *)
end





module Constructor:
sig
    type t

    val make: string -> Term.typ -> t
end


module Type:
sig
    type t

    val make: int -> Header.t -> Constructor.t array -> t
    (** [make nprevious header constructors] *)
end


type t

val make: params -> Fmlib.Common.Int_set.t -> Type.t array -> t

val up: int -> t -> t


val count_types: t -> int

val count_params: t -> int

val is_param_positive: int -> t -> bool

val parameter_name: int -> t -> string

val parameters: t -> params

val ith_type: int -> t -> string * Term.typ

val count_constructors: int -> t -> int

val count_previous_constructors: int -> t -> int

val raw_constructor: int -> int -> t -> string * Term.typ
(** [raw_constructor i j ind]

    The name and the type of the [j]th constructor of the [i]th inductive type
    of the family, valid in a context with the inductive types and the
    parameters.
*)

val constructor: int -> int -> t -> string * Term.typ
(** [constructor i j ind]

    The name and the type of the [j]th constructor of the [i]th inductive type
    of the family, valid in a context with the inductive types.
*)
