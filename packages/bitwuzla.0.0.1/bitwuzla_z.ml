(**
   [value_get_bv_bits term]
   get the bits of a given term.

   @param term The value term.

   @return The unsigned value of [term] as a Zarith integer.

   @raise Invalid_argument if the term is not a value.
*)
external value_get_bv_bits : Bitwuzla_c.term -> Z.t
  = "ocaml_bitwuzla_value_get_bv_bits"
