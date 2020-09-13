open SCaml

let main () (address, mutez) =
  let sender_address = Global.get_sender () in
  match (Contract.contract sender_address : unit contract) with
  | None -> failwith ()
  | Some c ->
      ([Operation.transfer_tokens () mutez c],
       (sender_address, GLobal.get_amount ()))

