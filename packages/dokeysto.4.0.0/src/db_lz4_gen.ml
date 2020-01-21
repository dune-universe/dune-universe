
(* values are LZ4-compressed marshaled-to-string values
   i.e. at encoding time we first marshal to string then LZ4 compress the
   marshaled value *)

(* just add compression/decompression of the values of the KV module *)
module KZV (KV: Dokeysto.Db_gen.Key_val) = struct
  
  let string_of_key = KV.string_of_key

  let key_of_string = KV.key_of_string

  let string_of_value (v: 'v): string =
    Db_lz4.compress (KV.string_of_value v)

  let value_of_string (s: string): 'v =
    KV.value_of_string (Db_lz4.uncompress s)

end

module ROZ (KV: Dokeysto.Db_gen.Key_val) = Dokeysto.Db_gen.RO (KZV (KV))

module RWZ (KV: Dokeysto.Db_gen.Key_val) = Dokeysto.Db_gen.RW (KZV (KV))
