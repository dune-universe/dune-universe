[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  let contract = Contract.implicit_account (Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx") in
  [ Operation.transfer_tokens () (Tz 10.0) contract ],
  ()
