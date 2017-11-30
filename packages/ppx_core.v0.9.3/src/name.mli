open! Import

(** [matches ~pattern name] returns [true] iff [name] matches [pattern].

    For instance, the exact set of names such that [matches ~pattern:"foo.bar.blah" name]
    is:
    - "foo.bar.blah"
    -     "bar.blah"
    -         "blah"
*)
val matches : pattern:string -> string -> bool

(** [fold_dot_suffixes "foo.bar.blah" ~init ~f] is
    [f "foo.bar.blah" (f "bar.blah" (f "blah" init)))]
*)
val fold_dot_suffixes : string -> init:'a -> f:(string -> 'a -> 'a) -> 'a

val get_outer_namespace : string -> string option

module Registrar : sig
  (** Names are organized by context. For instance contexts can be: expressions, patterns,
      types, ... *)
  type 'context t

  (** - [kind] is a description of the things registered. For instance: "extension",
      "attribute", ...

      - [current_file] is where this function is called. Must be [__FILE__].

      - [string_of_context]: human readable description of a context
  *)
  val create
    :  kind:string
    -> current_file:string (* must be [__FILE__] *)
    -> string_of_context:('context -> string option)
    -> 'context t

  val register : kind:[ `Attribute | `Extension ] -> 'context t -> 'context -> string -> unit

  val spellcheck :
    'context t -> 'context -> ?white_list:string list -> string -> string option

  val raise_errorf
    :  'context t
    -> 'context
    -> ?white_list:string list
    -> (string -> 'a, unit, string, 'c) format4
    -> string Loc.t
    -> 'a
end

module Whitelisted : sig
  val get_attribute_list : unit -> string list
  val get_extension_list : unit -> string list

  val is_whitelisted : kind:[ `Attribute | `Extension ] -> string -> bool
end

module Reserved_namespaces : sig
  (** [reserve "foo"] has two implications:
        - one can't then declare an attribute inside this namespace
        - attributes within this namespace won't be reported by [check_unused]

      This is here to insure that the rewriter cohabits well with other rewriter
      or tools (e.g. merlin) which might leave attribute on the AST.

      N.B. the "merlin" namespace is reserved by default. *)
  val reserve : string -> unit

  val is_in_reserved_namespaces : string -> bool
end

val comes_from_merlin : string -> bool
