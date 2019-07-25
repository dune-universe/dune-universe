val compare_be : Cstruct.t -> Cstruct.t -> int
(** Compare two cstructs, interpreting them as big endian numbers.
    Raises [Invalid_argument _] if they have a different length. *)
