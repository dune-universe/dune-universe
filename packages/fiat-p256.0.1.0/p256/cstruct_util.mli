val pp_hex_le : Format.formatter -> Cstruct.t -> unit
(** Display the contents of a cstruct as hex data, seen as a little endian
    number. *)

val compare_be : Cstruct.t -> Cstruct.t -> int
(** Compare two cstructs, interpreting them as big endian numbers.
    Raises [Invalid_argument _] if they have a different length. *)
