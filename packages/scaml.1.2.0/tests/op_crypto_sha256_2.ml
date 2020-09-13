[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  assert (Bytes "55c53f5d490297900cefa825d0c8e8e9532ee8a118abe7d8570762cd38be9818" = Crypto.sha256 (Bytes "0123456789ABCDEF"))

