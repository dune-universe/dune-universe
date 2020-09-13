[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  assert (Bytes "650161856da7d9f818e6047cf6b2092bc7aa3767d3495cfbefe2b710ed684a43ba933ea8286ef67d975e64e0482e5ebe0701788989396545b6badb3b0a136f19" = Crypto.sha512 (Bytes "0123456789ABCDEF"))



