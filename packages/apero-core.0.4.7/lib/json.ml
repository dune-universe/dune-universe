open Acommon.Result
module Json = struct

  type t = Yojson.Safe.t

  let to_string j = Yojson.Safe.to_string j

  let of_string s = Yojson.Safe.from_string s

  let validate s = 
    try
      let j = of_string s in
      let _ = Yojson.Safe.validate_json [] j in
      return j
    with
    | e -> 
      let (r:Atypes.error) = (`ValidationError (`Msg (Printexc.to_string e))) in 
      fail r

end
