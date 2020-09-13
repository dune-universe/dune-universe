(* 
STORAGE=(None : SCaml.key_hash option)
*)
[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  let key = Key "p2pk66uq221795tFxT7jfNmXtBMdjMf6RAaxRTwv1dbuSHbH6yfqGwz" in
  [], Some (Crypto.hash_key key) (* "tz3eMVQA1K4yw47g9nHxM37BJNmDyWtpGB3T" *)
