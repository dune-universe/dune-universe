open Physical_time 

module Time_64bit : Physical_time with type t = int64 = struct
  include Apero.Ordered.Make(Int64)

  let pow_2_32f = Int64.to_float 0x100000000L

  let frac_per_sec = 0x100000000L
  (* number of NTP fraction per second (2^32) *)

  let nano_per_sec = 1000000000L

  let ns_part t =
    let open Int64 in 
    let frac = logand t 0xffffffffL in
    div (mul frac nano_per_sec) frac_per_sec

  
  let to_seconds t =
    let open Int64 in
    let seconds = to_float @@ shift_right t 32 in
    let fraction = (to_float @@ logand t 0xffffffffL) /. pow_2_32f in
    seconds +. fraction

  let of_seconds f =
    let open Int64 in
    let seconds = shift_left (of_float f) 32 in
    let fraction = of_float @@ (Float.rem f 1.) *. pow_2_32f in
    add seconds fraction

  let after t t' = t' > t

  let before t t' = t' < t

  let to_string = Int64.to_string

  let of_string = Int64.of_string_opt

  let to_rfc3339 t =
    let tm = Unix.gmtime @@ to_seconds t in
    Printf.sprintf "%d-%02d-%02dT%02d:%02d:%02d.%09LdZ"
    (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec (ns_part t)


  let encode = Apero.encode_vle ?size:None

  let decode = Apero.decode_vle

  let pp ppf t = Format.fprintf ppf "%s" (to_rfc3339 t)

end
