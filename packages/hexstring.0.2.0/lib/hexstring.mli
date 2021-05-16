val encode : bytes -> string
(** encodes a bytestring into a hexstring *)

val decode : string -> (bytes, string) result
(** decodes an hexstring into a bytestring (can return an error) *)
