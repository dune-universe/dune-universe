[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  let key = Key "p2pk66uq221795tFxT7jfNmXtBMdjMf6RAaxRTwv1dbuSHbH6yfqGwz" in
  assert (
    Crypto.hash_key key =
    Crypto.hash_key key
  )
