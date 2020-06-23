open SCaml

let [@entry] deposit key_hash map = 
  let balance = match Map.get key_hash map with
    | None -> Tz 0.
    | Some tz -> tz
  in
  ( [], Map.update key_hash (Some (balance +$ Global.get_amount ())) map )

let [@entry] withdraw (key, amount, sign) map =
  (* We do not check AMOUNT.  We happily receive it. *)
  if not (Crypto.check_signature key sign (Obj.pack (key, amount))) then
    failwith "invalid signature";
  let key_hash = Crypto.hash_key key in
  let balance = match Map.get key_hash map with
    | None -> Tz 0.
    | Some tz -> tz
  in
  if balance < amount then failwith "not enough balance";
  let new_balance = balance -$ amount in
  let new_balance = if new_balance = Tz 0. then None else Some new_balance in
  ( [ Operation.transfer_tokens () amount (Contract.implicit_account key_hash) ], 
    Map.update key_hash new_balance map )

(* The original article's remark:

Important Note: This contract is vulnerable to replay attacks. I've ignored this vulnerability because users can only move funds to the contract and back to themselves. If you are implementing accounts that give users the ability to withdraw funds to another address, add an index for each user. The user should instead sign a pair, consisting of the integer and tez amount. The integer should be incremented after every transaction. If you do not do this, an attacker can replay a signed transaction an unlimited number of times.
*)
