(** Herald parser *)

(** Options read from the herald *)
type herald = {
    bnd : int option;
    lmt : int option;
    in_ord : unit option;
  }

(** [parse_herald] parses a herald line.  In returns [None] when
    herald line is malformed. *)
val parse_herald : (string * int option) list -> herald option

(** The empty herald line *)
val empty_herald : herald
