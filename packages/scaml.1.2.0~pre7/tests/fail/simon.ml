open SCaml

type storage = 
  { address : address
  ; tz : tz
  }

let main string storage =
  match (Contract.contract storage.address : string contract option) with
  | None ->
      let amount = Global.get_amount () in
      let sender_address = Global.get_sender () in
      begin match Contract.contract sender_address with
      | None -> failwith ()
      | Some sender_contract ->
          ( [Operation.transfer_tokens string amount sender_contract]
          , (* I think this one is wrong *)
            { address= sender_address
            ; tz= amount } 
          )
      end
  | Some contract ->
      let amount = Global.get_amount () in
      let sender_address = Global.get_sender () in
      ( [Operation.transfer_tokens string storage.tz contract]
      , { address= sender_address; tz= amount } )
