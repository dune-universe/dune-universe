(** Same as OCaml's Location.t *)
type t = {
  loc_start : Lexing.position;
  loc_end   : Lexing.position;
  loc_ghost : bool;
}

val format : Format.formatter -> t -> unit
