(** [ASN1_Counter64.t] encodes unsigned 64 bit values including counters and unsigned int-s.
    The type maps directly from the underlying C API representation *)
type t = {
  high : int
; low : int
}

val to_string : t -> string
