open Fmlib


module type GAMMA =
sig
    type t
    val is_valid_index: int -> t -> bool
    val name_of_index: int -> t -> string
    val push_local: string -> Term.typ -> t -> t
end



module Make (Gamma: GAMMA) (P: Pretty_printer.SIG):
sig
    val print: Inductive.t -> Gamma.t -> P.t
end


val string_of_inductive: Inductive.t -> Gamma.t -> string
