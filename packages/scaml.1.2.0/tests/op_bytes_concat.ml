[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  assert (
    Bytes "0123456789abcdef" 
    = Bytes.concat (Bytes "01234567") (Bytes "89ABCDEF")
  )
