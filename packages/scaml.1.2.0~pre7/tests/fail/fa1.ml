open SCaml

type storage = 
  { accounts : (address, nat) big_map
  ; totalSupply : nat 
  }

type ('a, 'r) view = ('a * 'r contract)

type transfer = 
  { from  : address 
  ; to_   : address
  ; value : nat
  }

exception NotAuthorized

let check_auth from =
  if Global.get_sender () <> from then raise NotAuthorized else ()

exception NotEnoughBalance of nat * nat

let [@entry] transfer { from; to_; value } s =
  check_auth from;
  let v_from = match BigMap.get from s.accounts with
    | None -> Nat 0
    | Some n -> n
  in
  let v_from'_int = v_from -^ value in
  let v_from' = match isnat v_from'_int with
    | None -> raise (NotEnoughBalance (value, v_from))
    | Some (Nat 0) -> None
    | Some v_from' -> Some v_from'
  in
  let v_to' = 
    match BigMap.get to_ s.accounts with
    | None -> Some value
    | Some n -> Some (n +^ value)
  in
  [],
  { s with 
    accounts= BigMap.update from v_from' (BigMap.update to_ v_to' s.accounts) }

type getBalance = (address, nat) view

let [@entry] getBalance (a, c : getBalance) s =
  let n = match BigMap.get a s.accounts with
    | None -> Nat 0
    | Some n -> n
  in
  [Operation.transfer_tokens n (Tz 0.) c], s

type getTotalSupply = (unit, nat) view

let [@entry] getTotalSupply ((), c : getTotalSupply) s =
  [Operation.transfer_tokens s.totalSupply (Tz 0.) c], s
           

