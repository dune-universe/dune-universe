open Ppxlib

(** Return the suffix to apply to a derived value name, based on the type name *)
val affix_from_type_name : kind: [`Prefix | `Suffix] -> string -> string

(** Return the core type describing the type declared in the given declaration.
    E.g. will return [[%type: 'a t]] for [type 'a t = A of int | B of 'a].
*)
val core_type_from_type_decl : loc: Location.t -> type_declaration -> core_type

(** Return whether the deriver is used in the context of ocamldep from the given
    expansion context
*)
val is_ocamldep : Expansion_context.Deriver.t -> bool

module Expr : sig
  (** Return the expression corresponding to the given variable name *)
  val var : loc: Location.t -> string -> expression

  (** Return the contructor expression with the given constructor name and argument expression *)
  val constructor : loc: Location.t -> constructor_name: string -> expression option -> expression
end

module List_ : sig
  (** [all_ok l] return [l'] if all elements of [l] are [Ok _] or the first encountered
      error otherwise.
  *)
  val all_ok : ('a, 'b) result list -> ('a list, 'b) result

  (** Apply the given function to the given list's elements until it returns [Ok _].
      Return [Error `Empty] if the list is empty or [Error (`Last err)] with the last error returned
      by [f] if it returned an error for every elements.
  *)
  val find_ok :
    f: ('a -> ('b, 'err) result) ->
    'a list ->
    ('b, [`Empty | `Last of 'err]) result
end

module Result_ : sig
  val (>|=) : ('a, 'err) result -> ('a -> 'b) -> ('b, 'err) result
end
