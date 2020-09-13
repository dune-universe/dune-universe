open SCaml

let [@entry] main () () =
  let op = Operation.transfer_tokens () (Tz 1.0) Contract.self in
  [ op; op ], ()
