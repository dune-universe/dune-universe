open Fmlib


module Name_map:
sig
  type t
  val count: t -> int
  val find:  string -> t -> int list
  val empty: t
  val add_unnamed: t -> t
  val add_local:  string -> t -> t
  val add_global: string -> t -> t
end

type t

val count: t -> int

val gamma: t -> Gamma.t
val name_map: t -> Name_map.t

val standard: unit -> t

val find_name: string -> t -> int list

val compute: Term.t -> t -> Term.t


module Pretty (P:Pretty_printer.SIG):
sig
  val print: Term.t -> t -> P.t
end
