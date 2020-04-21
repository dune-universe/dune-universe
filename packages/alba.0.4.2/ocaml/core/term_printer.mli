open Fmlib


(** Signature which must be satisfied by a valid context. *)
module type GAMMA =
sig
 type t
  val is_valid_index: int -> t -> bool
  val name_of_index: int -> t -> string
  val push_local: string -> Term.typ -> t -> t
end



(** Pretty printer which can print terms in a context with the use of apretty
printer. *)
module Pretty:
functor (G: GAMMA) (P:Pretty_printer.SIG) ->
sig
  val print: Term.t -> G.t -> P.t
  (** [print term gamma] Print [term] in the context [gamma] using the printer
  [P]. *)
end



(** Pretty printer which prints terms in a context into a string. *)
module String_print:
functor (G: GAMMA) ->
sig
  val string_of_term: Term.t -> G.t -> string
  (** [string_of_term term gamma]. Pretty print [term] in the context [gamma]
  into a string. *)
end



val string_of_term: Term.t -> Gamma.t -> string
