open SCaml
    
let main () () =
(*
  let add_transfer_bad ops c = Operation.transfer_tokens () (Tz 1.0) c :: ops in
*)
  let add_transfer (ops, c) = Operation.transfer_tokens () (Tz 1.0) c :: ops in
  [], ()
