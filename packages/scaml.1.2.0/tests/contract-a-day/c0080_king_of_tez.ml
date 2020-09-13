open SCaml

type storage = 
  { deadline : Timestamp.t
  ; amount : tz
  ; address : Address.t
  }

(* XXX The function is somehow expanded in .tz *)
let make_king now amount source =
  { deadline= Timestamp.add now (Int 1209600) (* 2 weeks *)
  ; amount
  ; address= source }

let main () storage =
  let source = Global.get_source () in
  let now = Global.get_now () in
  let amount = Global.get_amount () in
  if now > storage.deadline then
    ([], make_king now amount source)
  else begin
    if amount <= storage.amount then failwith "failed to be the king";
    ( [ Operation.transfer_tokens () amount 
          (* XXX We want to have from_Some *)
          (match Contract.contract storage.address with
           | None -> failwith "invalid contract"
           | Some c -> c
          ) 
      ],
      make_king now amount source )
  end
