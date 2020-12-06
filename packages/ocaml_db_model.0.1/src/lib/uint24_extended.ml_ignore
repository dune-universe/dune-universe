open Stdint
module Uint24_extended = struct
  include Uint24
			 
  let pp fmt t = (Format.fprintf fmt "%s") (to_string t)
  let show t = Uint24.to_string t

  let pp_uint24 = pp
  let show_uint24 = show

  let equal_uint24 t1 t2 = if Uint24.compare t1 t2 == 0 then true else false
  let compare_uint24 t1 t2 = Uint24.compare t1 t2
  let equal = equal_uint24
  let compare = compare_uint24

  let to_yojson t =
    let s = Uint24.to_string t in
    let s2 = String.concat "" ["{uint24:";s;"}"] in
    Yojson.Safe.from_string s2;;
    
  let of_yojson j =
    try
      let s = Yojson.Safe.to_string j in
      let splits = String.split_on_char ':' s in
      let value_half = List.nth splits 1 in
      let rbracket_i = String.index value_half '}' in 
      let value = String.sub value_half 0 rbracket_i in 
      Core.Result.Ok (Uint24.of_string value)
    with err -> Error "uint24_extended::of_yojson() failed.";;

end 
