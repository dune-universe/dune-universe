open Alba2_common

module Term = Term2

module Document = Pretty_printer2.Document

module type CONTEXT =
  sig
    type t
    val empty: t
    val push: Feature_name.t option -> Term.typ -> t -> t
    val push_simple: string option -> Term.typ -> t -> t
    val push_arguments: Term.arguments -> t -> t
    val push_fixpoint: Term.fixpoint -> t -> t
    val is_valid: int -> t -> bool
    val name: int -> t -> Feature_name.t option
  end

module type S =
  functor (C:CONTEXT) ->
  sig
    type context = C.t

    type level
    val compact: level
    val all_types: level
    val detailed: level

    val term: context -> level -> Term.t -> Document.t
    val fixpoint: context -> level -> Term.fixpoint -> Document.t
  end


module Make: S
