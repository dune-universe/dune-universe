module CoreInt32 = Core.Int32
module CoreInt32_extended = struct

    include CoreInt32

    let show t = Core.Int32.to_string_hum ~delimiter:',' t

    let to_yojson t =
      let s = Core.Int32.to_string t in
      let s = Core.String.concat ["{CoreInt32_extended:";s;"}"] in 
      Yojson.Safe.from_string s;;
      
    let of_yojson j =
      try
	let s = Yojson.Safe.to_string j in
	let splits = String.split_on_char ':' s in
	let value_half = List.nth splits 1 in
	let rbracket_i = String.index value_half '}' in 
	let value = String.sub value_half 0 rbracket_i in
	let i = CoreInt32.of_string value in
	Ok i
      with _err -> Error "CoreInt32_extended::of_yojson() failed.";;
end 
