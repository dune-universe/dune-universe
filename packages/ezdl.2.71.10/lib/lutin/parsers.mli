(** SYNTAXE : interface du parser mlyacc


*)


val read_lut : string list -> Syntaxe.package

(* Necessary to be able to interpret several lutin programs in the
   same main (as lurettetop does) *)
val reinit_parser : unit -> unit

(*/*)
(* For test purposes *)
val lexemize_lut : string -> unit
